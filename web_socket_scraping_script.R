# web sockets are a lot trickier than web service calls because they're an open
# connection that returns data asynchronously.  Essentially, you send a message
# and receive a response, and can define the behavior to the response on your
# end.

# My method is to basically loop through all the calls I want to make with a
# slight delay between each call and save all the server responses to a csv file

player_season_combos <-
  # csv file of all players to play a regular season game since 2021-22
  # downloaded from evolving hockey
  # used to get the API ID for each player instead of using roster reports for
  # each game because it was easier
  readr::read_csv("eh_skaters.csv") |>
    dplyr::transmute(
      player = Player,
      id = `API ID`,
      season =
        glue::glue(
          "{
              stringr::str_sub(Season, end = 2) |> as.numeric() |> magrittr::add(2000)
          }{
            stringr::str_sub(Season, start = 4) |> as.numeric() |> magrittr::add(2000)
          }"
        ),
      position = Position,
      dob = Birthday,
      stage = "regular"
    )

# each web service call is broken down into sections and strength states, this
# gets all valid combos
ws_call_combos <-
  tidyr::expand_grid(
    sectionName = c("skatingdistance", "zonetime"),
    manpower = c("all", "es", "pp", "pk")
  ) |>
  dplyr::bind_rows(
    tidyr::expand_grid(
      sectionName = c("skatingspeed", "shotspeed"),
      manpower = "all"
    )
  )

# this combines each section combo with each api id and season the player played
player_season_combos <-
  player_season_combos |>
  dplyr::group_by(player, id, season, position, dob, stage) |>
  dplyr::mutate(
    ws_call_combos =
      purrr::map(
        T,
        function(x) {
          ws_call_combos
        }
      )
  ) |>
  tidyr::unnest(ws_call_combos) |>
  dplyr::mutate(
    # this is a return value the server sends with each response.  I can set the
    # value on my end so I can identify what player/season/section I'm making
    # the call for
    target = "{sectionName}_{manpower}_{stage}-{id}-{season}" |>
      glue::glue()
  )

# open the web socket connection
ws <- websocket::WebSocket$new("wss://edge.nhl.com/en/skater/")

# define the message event for the web socket
# there is support for error handling, but I haven't worked out how to implement
# that yet
ws$onMessage(function(event) {
  if (!is.null(event)) {
    # the websocket returns the data in json format
    data <- jsonlite::fromJSON(event$data)

    if (!is.null(data)) {
      # this is the target from line 63, I split by hyphen (-) to identify
      # which call this is a response to
      target <-
        data$target |>
        stringr::str_split("-") |>
        purrr::flatten_chr()

      if (stringr::str_detect(target[1], "skatingdistance")) {
        data$html |>
          rvest::read_html() |>
          rvest::html_element("#skatingdistance-datebarchart") |>
          rvest::html_attr("data-json") |>
          jsonlite::fromJSON() |>
          purrr::pluck("chartData") |>
          tibble::tibble() |>
          dplyr::transmute(
            id = target[2],
            season = target[3],
            date,
            value,
            tooltip =
              purrr::map(
                tooltip,
                function(x) {
                  x |>
                    tibble::enframe(name = NULL, value = "tooltip") |>
                    dplyr::mutate(desc = c("dist", "game", "toi")) |>
                    tidyr::pivot_wider(values_from = tooltip, names_from = desc) |>
                    dplyr::select(game, toi)
                }
              )
          ) |>
          tidyr::unnest(tooltip) |>
          # use the target again to write to a csv to save every response
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)

        ####
        ## Important! The csv files won't start writing untill all the messages
        ## have been sent to the server and the responses are received!  I
        ## don't know the technical reason for this, but for whatever reason,
        ## the response is not 1:1.  Sometimes responses are sent twice as well.
        ## I'll repeat this below too
        ####
      } else if (stringr::str_detect(target[1], "zonetime")) {
        data$html |>
          rvest::read_html() |>
          rvest::html_element("#zonetime-zonechart") |>
          rvest::html_attr("data-json") |>
          jsonlite::fromJSON() |>
          purrr::pluck("chartData") |>
          tibble::tibble() |>
          dplyr::transmute(
            id = target[2],
            season = target[3],
            zone,
            zonetime_perc = abs(value) / 100
          ) |>
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else if (stringr::str_detect(target[1], "skatingspeed")) {
        html <-
          data$html |>
          rvest::read_html()

        max_speed_date <-
          html |>
          rvest::html_element("span") |>
          rvest::html_attr("data-tooltip")

        html |>
          rvest::html_table() |>
          purrr::flatten_df() |>
          dplyr::select(X1, X2) |>
          # print() |>
          tail(-1) |>
          # print() |>
          dplyr::mutate(X2 = X2 |> as.character() |> stringr::str_remove(",") |> as.numeric()) |>
          tidyr::pivot_wider(names_from = X1, values_from = X2) |>
          dplyr::mutate(
            id = target[2],
            season = target[3],
            max_speed_date = max_speed_date
          ) |>
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else if (stringr::str_detect(target[1], "shotspeed")) {
        html <-
          data$html |>
          rvest::read_html()

        max_speed_date <-
          html |>
          rvest::html_element("span") |>
          rvest::html_attr("data-tooltip")

        html |>
          rvest::html_table() |>
          purrr::flatten_df() |>
          tail(-1) |>
          dplyr::select(X1, X2) |>
          dplyr::mutate(X2 = X2 |> as.character() |> stringr::str_remove(",") |> as.numeric()) |>
          tidyr::pivot_wider(names_from = X1, values_from = X2) |>
          dplyr::mutate(
            id = target[2],
            season = target[3],
            max_speed_date = max_speed_date
          ) |>
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else {
        print("section not coded yet")
      }
    } else {
      print("NULL data")
    }
  } else {
    print("event returned NULL")
  }
})

# this is the loop to make all the web service calls.  I like purrr cause it
# feels more tidyverse, but a for loop would work just as well
player_season_combos |>
  # I like to test my functions using head() which will pull the first six
  # rows/items
  # head() |>
  dplyr::mutate(
    scrape_attempted =
      purrr::pmap_lgl(
        list(
          season = season,
          id = id,
          target = target,
          sectionName = sectionName,
          manpower = manpower,
          stage = stage
        ),
        function(season, id, target, sectionName, manpower, stage) {
          # create the message payload in json format
          send_json <-
            '{
              "type":"action",
              "event":{
                "domain":"edge.nhl.com",
                "uri":"/en/skater/{{season}}-regular-{{id}}",
                "action":"load",
                "data":{
                  "renderFunction":"renderProfileContent",
                  "target":"{{target}}",
                  "params":{
                    "sectionName":"{{sectionName}}",
                    "units":"imperial",
                    "manpower":"{{manpower}}",
                    "season":"{{season}}",
                    "stage":"{{stage}}",
                    "feed":"skatersProfiles",
                    "id":"{{id}}"
                  },
                "callbackFunction":"runClientFns"
                }
              }
            }' |>
            # because json uses curly braces, I set the glue braces to double
            # braces
            glue::glue(.open = "{{", .close = "}}")

          ws$send(send_json)

          # small delay between calls to keep the server from overloading
          Sys.sleep(0.5)

          T
        }
      )
  )

####
## Important! The csv files won't start writing untill all the messages
## have been sent to the server and the responses are received!  I
## don't know the technical reason for this, but for whatever reason,
## the response is not 1:1.  Sometimes responses are sent twice as well.
## Don't close the connection (line 262) until the csv files are done writing!
####

# close the connection when you're done
ws$close()





#####
## Playoffs
###

player_season_combos_playoffs <-
  readr::read_csv("eh_skaters_playoffs.csv") |>
  dplyr::transmute(
    player = Player,
    id = `API ID`,
    season =
      glue::glue(
        "{
              stringr::str_sub(Season, end = 2) |> as.numeric() |> magrittr::add(2000)
          }{
            stringr::str_sub(Season, start = 4) |> as.numeric() |> magrittr::add(2000)
          }"
      ),
    position = Position,
    dob = Birthday,
    stage = "playoffs"
  )

player_season_combos_playoffs <-
  player_season_combos_playoffs |>
  dplyr::group_by(player, id, season, position, dob, stage) |>
  dplyr::mutate(
    ws_call_combos =
      purrr::map(
        T,
        function(x) {
          ws_call_combos
        }
      )
  ) |>
  tidyr::unnest(ws_call_combos) |>
  dplyr::mutate(
    target = "{sectionName}_{manpower}_{stage}-{id}-{season}" |>
      glue::glue()
  )

ws <- websocket::WebSocket$new("wss://edge.nhl.com/en/skater/")

ws$onMessage(function(event) {
  if (!is.null(event)) {
    data <- jsonlite::fromJSON(event$data)

    if (!is.null(data)) {
      target <-
        data$target |>
        stringr::str_split("-") |>
        purrr::flatten_chr()

      if (stringr::str_detect(target[1], "skatingdistance")) {
        data$html |>
          rvest::read_html() |>
          rvest::html_element("#skatingdistance-datebarchart") |>
          rvest::html_attr("data-json") |>
          jsonlite::fromJSON() |>
          purrr::pluck("chartData") |>
          tibble::tibble() |>
          dplyr::transmute(
            id = target[2],
            season = target[3],
            date,
            value,
            tooltip =
              purrr::map(
                tooltip,
                function(x) {
                  x |>
                    tibble::enframe(name = NULL, value = "tooltip") |>
                    dplyr::mutate(desc = c("dist", "game", "toi")) |>
                    tidyr::pivot_wider(values_from = tooltip, names_from = desc) |>
                    dplyr::select(game, toi)
                }
              )
          ) |>
          tidyr::unnest(tooltip) |>
          # print()
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else if (stringr::str_detect(target[1], "zonetime")) {
        data$html |>
          rvest::read_html() |>
          rvest::html_element("#zonetime-zonechart") |>
          rvest::html_attr("data-json") |>
          jsonlite::fromJSON() |>
          purrr::pluck("chartData") |>
          tibble::tibble() |>
          dplyr::transmute(
            id = target[2],
            season = target[3],
            zone,
            zonetime_perc = abs(value) / 100
          ) |>
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else if (stringr::str_detect(target[1], "skatingspeed")) {
        html <-
          data$html |>
          rvest::read_html()

        max_speed_date <-
          html |>
          rvest::html_element("span") |>
          rvest::html_attr("data-tooltip")

        html |>
          rvest::html_table() |>
          purrr::flatten_df() |>
          dplyr::select(X1, X2) |>
          tail(-1) |>
          dplyr::mutate(X2 = X2 |> as.character() |> stringr::str_remove(",") |> as.numeric()) |>
          tidyr::pivot_wider(names_from = X1, values_from = X2) |>
          dplyr::mutate(
            id = target[2],
            season = target[3],
            max_speed_date = max_speed_date
          ) |>
          # print()
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else if (stringr::str_detect(target[1], "shotspeed")) {
        html <-
          data$html |>
          rvest::read_html()

        max_speed_date <-
          html |>
          rvest::html_element("span") |>
          rvest::html_attr("data-tooltip")

        html |>
          rvest::html_table() |>
          purrr::flatten_df() |>
          tail(-1) |>
          dplyr::select(X1, X2) |>
          dplyr::mutate(X2 = X2 |> as.character() |> stringr::str_remove(",") |> as.numeric()) |>
          tidyr::pivot_wider(names_from = X1, values_from = X2) |>
          dplyr::mutate(
            id = target[2],
            season = target[3],
            max_speed_date = max_speed_date
          ) |>
          # print()
          readr::write_csv("{target[1]}.csv" |> glue::glue(), append = T)
      } else {
        print("section not coded yet")
      }
    } else {
      print("NULL data")
    }
  } else {
    print("event returned NULL")
  }
})

player_season_combos_playoffs |>
  # head(10) |>
  dplyr::mutate(
    scrape_attempted =
      purrr::pmap_lgl(
        list(
          season = season,
          id = id,
          target = target,
          sectionName = sectionName,
          manpower = manpower,
          stage = stage
        ),
        function(season, id, target, sectionName, manpower, stage) {
          send_json <-
            '{
              "type":"action",
              "event":{
                "domain":"edge.nhl.com",
                "uri":"/en/skater/{{season}}-regular-{{id}}",
                "action":"load",
                "data":{
                  "renderFunction":"renderProfileContent",
                  "target":"{{target}}",
                  "params":{
                    "sectionName":"{{sectionName}}",
                    "units":"imperial",
                    "manpower":"{{manpower}}",
                    "season":"{{season}}",
                    "stage":"{{stage}}",
                    "feed":"skatersProfiles",
                    "id":"{{id}}"
                  },
                "callbackFunction":"runClientFns"
                }
              }
            }' |>
            glue::glue(.open = "{{", .close = "}}")

          ws$send(send_json)

          Sys.sleep(0.5)

          T
        }
      )
  )

ws$close()




