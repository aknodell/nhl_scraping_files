get_game_rosters_html <- function(gm_id) {
  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  "https://www.nhl.com/scores/htmlreports/{season}/RO0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html()
}

get_game_shifts_html <- function(gm_id, side) {
  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  "https://www.nhl.com/scores/htmlreports/{season}/T{ifelse(side == 'home', 'H', 'V')}0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html()
}

get_game_pbp_html <- function(gm_id) {
  season <-
    gm_id |>
    as.character() |>
    stringr::str_sub(end = 4)

  season <-
    "{season}{as.integer(season) + 1}" |>
    glue::glue()

  game_id <- gm_id |> stringr::str_sub(start = -5)

  "https://www.nhl.com/scores/htmlreports/{season}/PL0{game_id}.HTM" |>
    glue::glue() |>
    rvest::read_html()
}

extract_game_metadata_html <- function(html, gm_id) {
  message("extracting metadata (html)")
  tables <- html |> rvest::html_table()

  tibble::tibble(
    game_id = gm_id |> as.integer(),
    attendance =
      tables[[6]] |>
      dplyr::pull(X1) |>
      stringr::str_to_lower() |>
      purrr::keep(stringr::str_detect, pattern = "attendance") |>
      stringr::str_extract("(\\d+,)*\\d+") |>
      stringr::str_remove_all(",") |>
      as.integer()
  )
}

extract_rosters_html <- function(html, gm_id) {
  message("extracting rosters (html)")
  tables <- html |> rvest::html_table()

  tables[[11]] |>
    tail(-1) |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
      letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
      venue = "away",
      team = tables[[9]] |> dplyr::pull(X1),
      sweater_number = X1 |> as.integer(),
      position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
      position = X2
    ) |>
    dplyr::bind_rows(
      tables[[12]] |>
        tail(-1) |>
        dplyr::transmute(
          game_id = gm_id |> as.integer(),
          name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
          letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
          venue = "home",
          team = tables[[9]] |> dplyr::pull(X2),
          sweater_number = X1 |> as.integer(),
          position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
          position = X2
        )
    )
}

extract_scratches_html <- function(html, gm_id) {
  message("extracting scratches (html)")
  tables <- html |> rvest::html_table()

  away_scratches <- tables[[13]] |> tail(-1)
  home_scratches <- tables[[14]] |> tail(-1)

  if (nrow(away_scratches) > 0) {
    away_scratches <-
      away_scratches |>
      dplyr::transmute(
        game_id = gm_id |> as.integer(),
        name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
        letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
        venue = "away",
        team = tables[[9]] |> dplyr::pull(X1),
        sweater_number = X1 |> as.integer(),
        position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
        position = X2
      )
  } else {
    away_scratches <- tibble::tibble(game_id = integer(0))
  }

  if (nrow(home_scratches) > 0) {
    home_scratches <-
      home_scratches |>
      dplyr::transmute(
        game_id = gm_id |> as.integer(),
        name = X3 |> stringr::str_remove("\\(.\\)") |> stringr::str_trim(),
        letter = X3 |> stringr::str_extract("\\(.\\)") |> stringr::str_remove_all("[\\(\\)]") |> stringr::str_trim(),
        venue = "home",
        team = tables[[9]] |> dplyr::pull(X2),
        sweater_number = X1 |> as.integer(),
        position_category = ifelse(X2 %in% c("D", "G"), X2, "F"),
        position = X2
      )
  } else {
    home_scratches <- tibble::tibble(game_id = integer(0))
  }

  dplyr::bind_rows(
    away_scratches,
    home_scratches
  )
}

extract_coaches_html <- function(html, gm_id) {
  message("extracting coaches (html)")
  tables <- html |> rvest::html_table()

  tibble::tibble(
    game_id = gm_id |> as.integer(),
    name = c(tables[[15]]$X1, tables[[16]]$X1),
    venue = c("away", "home"),
    team = c(tables[[9]]$X1, tables[[9]]$X2)
  )
}

extract_referees_html <- function(html, gm_id) {
  message("extracting referees (html)")
  tables <- html |> rvest::html_table()

  tables[[18]] |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      name = X1 |> stringr::str_remove("#\\d+") |> stringr::str_trim()
    )
}

extract_linesmen_html <- function(html, gm_id) {
  message("extracting linesmen (html)")
  tables <- html |> rvest::html_table()

  tables[[19]] |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      name = X1 |> stringr::str_remove("#\\d+") |> stringr::str_trim()
    )
}

extract_shifts_html <- function(html, gm_id, side) {
  message("extracting {side} shifts (html)" |> glue::glue())
  html |>
    rvest::html_table() |>
    purrr::pluck(10) |>
    dplyr::select(X1:X4) |>
    dplyr::mutate(
      X1 = ifelse(stringr::str_detect(X1, "Shift|Per|Total|(\\d$)"), NA_character_, X1)
    ) |>
    tidyr::fill(X1, .direction = "down") |>
    dplyr::filter(stringr::str_count(X3, "\\d+:\\d+") == 2) |>
    dplyr::transmute(
      game_id = gm_id |> as.integer(),
      venue = side,
      sweater_number = X1 |> stringr::str_extract("\\d+") |> as.integer(),
      game_period =
        X2 |>
        stringr::str_replace_all("OT", "4") |>
        as.integer(),
      shift_start =
        purrr::map2_dbl(
          X3,
          game_period,
          function(t, p) {
            t |>
              stringr::str_extract("^\\d+:\\d+") |>
              stringr::str_split(":") |>
              purrr::flatten_chr() |>
              as.integer() |>
              magrittr::multiply_by(c(60, 1)) |>
              sum() |>
              magrittr::add((p - 1) * 1200)

          }
        ),
      shift_end =
        purrr::map2_dbl(
          X4,
          game_period,
          function(t, p) {
            t |>
              stringr::str_extract("^\\d+:\\d+") |>
              stringr::str_split(":") |>
              purrr::flatten_chr() |>
              as.integer() |>
              magrittr::multiply_by(c(60, 1)) |>
              sum() |>
              magrittr::add((p - 1) * 1200)

          }
        )
    )
}

extract_pbp_html <- function(html, gm_id) {
  message("extracting play-by-play (html)")
  purrr::map(
    c(".evenColor", ".oddColor"),
    function(class) {
      html |>
        rvest::html_elements(class) |>
        purrr::map(
          function(r) {
            tds <-
              r |>
              rvest::html_elements("td")

            event_details <-
              tibble::tibble(
                name = c("event_id", "game_period", "event_strength", "game_seconds", "event_type", "event_description"),
                value =
                  tds |>
                  head(6) |>
                  rvest::html_text2() |>
                  stringr::str_squish()
              )

            away_on <-
              tibble::tibble(
                value =
                  tds[7] |>
                  rvest::html_elements("font") |>
                  rvest::html_text2()
              ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "away_on_{rowid}" |>
                  glue::glue()
              )

            home_on <-
              tibble::tibble(
                value =
                  r |>
                  rvest::html_elements("td:nth-child(8)") |>
                  rvest::html_elements("table") |>
                  rvest::html_elements("font") |>
                  rvest::html_text2()
              ) |>
              tibble::rowid_to_column() |>
              dplyr::mutate(
                name =
                  "home_on_{rowid}" |>
                  glue::glue()
              )

            dplyr::bind_rows(
              event_details,
              home_on,
              away_on
            ) |>
              dplyr::select(-rowid) |>
              tidyr::pivot_wider(names_from = name, values_from = value)
          }
        ) |>
        dplyr::bind_rows()
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr::transmute(
      game_id = as.integer(gm_id),
      event_id = as.integer(event_id),
      game_period = as.integer(game_period),
      game_seconds =
        purrr::map2_int(
          game_seconds,
          game_period,
          function(t, p) {
            t |>
              stringr::str_extract("^\\d+:\\d+") |>
              stringr::str_split(":") |>
              purrr::flatten_chr() |>
              as.integer() |>
              magrittr::multiply_by(c(60, 1)) |>
              sum() |>
              magrittr::add((p - 1) * 1200)
          }
        ),
      event_strength =
        ifelse(stringr::str_trim(event_strength) == "", NA_character_, event_strength),
      event_type,
      event_description,
      dplyr::across(
        .cols = -c(event_id:event_description),
        .fns = function(x) {as.integer(x)}
      )
    ) |>
    dplyr::arrange(event_id)
}

get_game_details_html <- function(gm_id) {
  message("getting game {gm_id} details (html)" |> glue::glue())
  gm_id <- gm_id |> as.character()

  roster_html <- get_game_rosters_html(gm_id)
  home_shifts_html <- get_game_shifts_html(gm_id, "home")
  away_shifts_html <- get_game_shifts_html(gm_id, "away")
  pbp_html <- get_game_pbp_html(gm_id)

  list(
    meta = roster_html |> extract_game_metadata_html(gm_id),
    rosters = roster_html |> extract_rosters_html(gm_id),
    scratches = roster_html |> extract_scratches_html(gm_id),
    coaches = roster_html |> extract_coaches_html(gm_id),
    referees = roster_html |> extract_referees_html(gm_id),
    linesmen = roster_html |> extract_linesmen_html(gm_id),
    shifts =
      dplyr::bind_rows(
        extract_shifts_html(home_shifts_html, gm_id, "home"),
        extract_shifts_html(away_shifts_html, gm_id, "away")
      ),
    pbp = pbp_html |> extract_pbp_html(gm_id)
  )
}
