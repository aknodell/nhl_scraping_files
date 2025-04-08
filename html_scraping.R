# There's also info in the HTML page about scratches, coaches, and officials,
# But I've just pulled the playing roster here

get_html_roster_report <- function(gm_id) {
  message(gm_id)

  # convert the game id to a string in case it was passed as a numeric
  # it will be easier to extract season and game id from a string than an
  # integer
  gm_id <- gm_id |> as.character()

  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  # glue lets you run R code within the brackets and converts the output to a
  # string
  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  tables <-
    "https://www.nhl.com/scores/htmlreports/{season}/RO0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html() |>
    rvest::html_table()

  # The roster reports are pretty standardized, so I hardcoded all the indexes
  # of the tables.  I'm not 100 sure they're all in the same format though

  game_date <-
    tables |>
    purrr::pluck(6) |>
    dplyr::pull(X1) |>
    # this extracts the game date using a regex pattern
    purrr::keep(
      stringr::str_detect,
      # run these three lines by itself to see the regex used to extract the
      # game date
      stringr::str_c(
        stringr::str_c(month.name, collapse = "|"),
        " \\d+, \\d{4}"
      )
    ) |>
    lubridate::mdy()

  teams <-
    tables |>
    purrr::pluck(9)

  home_skaters <-
    tables |>
    purrr::pluck(12) |>
    # remove the first row because it's just column names
    tail(-1) |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      game_date = game_date,
      team = teams$X2,
      venue = "home",
      # stringr::str_squish will remove leading/trailing white space and
      # compress (squish) any repeated white space in the middle of the string
      sweaterNuber = stringr::str_squish(X1),
      position = stringr::str_squish(X2),
      player = stringr::str_squish(X3)
    )

  away_skaters <-
    tables |>
    purrr::pluck(11) |>
    tail(-1) |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      game_date = game_date,
      team = teams$X1,
      venue = "away",
      sweaterNuber = stringr::str_squish(X1),
      position = stringr::str_squish(X2),
      player = stringr::str_squish(X3)
    )

  dplyr::bind_rows(home_skaters, away_skaters)
}

# This code is super slow, I'm planning to refactor it sometime, but just a
# heads up that it takes a long time to run for a single game, about 3 minutes

# The HTML format is a mess of nested tables within tables.  I just extract
# time, event, event_description, strength state, and sweater numbers of the
# players on ice for the home/away teams

get_html_play_by_play <- function(gm_id) {
  message(gm_id)

  # convert the game id to a string in case it was passed as a numeric
  # it will be easier to extract season and game id from a string than an
  # integer
  gm_id <- gm_id |> as.character()

  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  # glue lets you run R code within the brackets and converts the output to a
  # string
  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  pbp_html <-
    "https://www.nhl.com/scores/htmlreports/{season}/PL0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html()

  html_scrape_result <-
    tibble::tibble(
      row_html =
        {
          l <-
            pbp_html |>
            rvest::html_elements(".tablewidth") |>
            rvest::html_elements(".evenColor") |>
            length()

          purrr::map(
            seq(l),
            function(r) {
              pbp_html |>
                rvest::html_elements(".tablewidth") |>
                rvest::html_elements(".evenColor") |>
                purrr::pluck(r)
            }
          )
        }
    ) |>
    dplyr::bind_rows(
      tibble::tibble(
        row_html =
          {
            l <-
              pbp_html |>
              rvest::html_elements(".tablewidth") |>
              rvest::html_elements(".oddColor") |>
              length()

            purrr::map(
              seq(l),
              function(r) {
                pbp_html |>
                  rvest::html_elements(".tablewidth") |>
                  rvest::html_elements(".oddColor") |>
                  purrr::pluck(r)
              }
            )
          }
      )
    ) |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      event_id =
        purrr::map_int(
          row_html,
          function(r) {
            r |>
              rvest::html_element("td:nth-child(1)") |>
              rvest::html_text2() |>
              as.integer()
          }
        ),
      period =
        purrr::map_int(
          row_html,
          function(r) {
            r |>
              rvest::html_element("td:nth-child(2)") |>
              rvest::html_text2() |>
              as.integer()
          }
        ),
      event_strength =
        purrr::map_chr(
          row_html,
          function(r) {
            r |>
              rvest::html_element("td:nth-child(3)") |>
              rvest::html_text2() |>
              stringr::str_trim()
          }
        ),
      clock =
        purrr::map_chr(
          row_html,
          function(r) {
            r |>
              rvest::html_element("td:nth-child(4)") |>
              rvest::html_text2() |>
              stringr::str_trim()
          }
        ),
      event_type =
        purrr::map_chr(
          row_html,
          function(r) {
            r |>
              rvest::html_element("td:nth-child(5)") |>
              rvest::html_text2() |>
              stringr::str_trim()
          }
        ),
      event_description =
        purrr::map_chr(
          row_html,
          function(r) {
            r |>
              rvest::html_element("td:nth-child(6)") |>
              rvest::html_text2() |>
              stringr::str_trim()
          }
        ),
      home_on_1 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(1)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      home_on_2 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(3)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      home_on_3 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(5)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      home_on_4 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      home_on_5 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(9)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      home_on_6 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(11)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      home_on_7 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(8)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(13)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_1 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(1)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_2 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(3)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_3 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(5)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_4 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_5 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(9)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_6 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(11)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        ),
      away_on_7 =
        purrr::map_int(
          row_html,
          function(r) {
            ret <-
              r |>
              rvest::html_elements("td:nth-child(7)") |>
              rvest::html_elements("table") |>
              rvest::html_elements("td:nth-child(13)") |>
              rvest::html_elements("table:nth-child(1)") |>
              rvest::html_text2() |>
              stringr::str_extract("\\d+") |>
              as.integer()

            ifelse(length(ret) == 0, NA_integer_, ret)
          }
        )
    ) |>
    tidyr::separate(
      clock,
      into = c("timeInPeriod", "timeRemaining"),
      sep = "\n"
    ) |>
    dplyr::mutate(
      game_seconds =
        purrr::map2_int(
          timeInPeriod,
          period,
          function(time, p) {
            time |>
              stringr::str_extract_all("\\d+") |>
              purrr::flatten_chr() |>
              as.integer() |>
              magrittr::multiply_by(c(60, 1)) |>
              sum() |>
              magrittr::add((p - 1) * 1200)
          }
        )
    ) |>
    dplyr::arrange(event_id)
}

# You can join the html play by play with the roster details pulled from the
# API to get the api ids that match each sweater number using the following code

game_id <- 2024021049
get_html_play_by_play(game_id) |>
  tidyr::pivot_longer(
    home_on_1:away_on_7
  ) |>
  dplyr::mutate(venue = stringr::str_sub(name, end = 4)) |>
  dplyr::left_join(
    get_rosters(game_id) |>
      dplyr::select(venue, sweater_number, api_id),
    by = c("venue" = "venue", "value" = "sweater_number")
  ) |>
  dplyr::mutate(value = api_id) |>
  tidyr::pivot_wider(
    id_cols = c(game_id:event_description, game_seconds),
    names_from = name, values_from = value
  )

# I haven't looked at scraping the HTML shift charts yet, but I think it would
# be similarly complicated to the play-by-play
