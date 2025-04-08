# Basically the same as the player level scraping script.  See that file for
# more details

team_season_combos <-
  readr::read_csv("game_logs_clean/skatingdistance_all_regular_clean.csv") |>
  dplyr::select(team, season) |>
  dplyr::filter(season < 20242025) |>
  dplyr::distinct() |>
  dplyr::mutate(id = team, stage = "regular")

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

team_season_combos <-
  team_season_combos |>
  dplyr::group_by(team, id, season, stage) |>
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

ws <- websocket::WebSocket$new("wss://edge.nhl.com/en/team/")

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
          readr::write_csv("team_{target[1]}.csv" |> glue::glue(), append = T)
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
          readr::write_csv("team_{target[1]}.csv" |> glue::glue(), append = T)
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
          readr::write_csv("team_{target[1]}.csv" |> glue::glue(), append = T)
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
          readr::write_csv("team_{target[1]}.csv" |> glue::glue(), append = T)
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

team_season_combos |>
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
                "uri":"/en/team/{{id}}",
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
                    "feed":"teamsProfiles",
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
          # print()
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






