get_game_pbp_api <- function(gm_id) {
  "https://api-web.nhle.com/v1/gamecenter/{gm_id}/play-by-play" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
}

get_game_shifts_api <- function(gm_id) {
  "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={gm_id}" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
}

extract_game_metadata_api <- function(pbp_json) {
  message("extracting metadata (api)")
  tibble::tibble(
    game_id = pbp_json$id,
    season = pbp_json$season,
    game_date = pbp_json$gameDate |> lubridate::as_date(),
    start_time_utc = pbp_json$startTimeUTC |> lubridate::as_datetime(),
    session = pbp_json$gameType,
    venue_name = pbp_json$venue$default,
    venue_place_name = pbp_json$venueLocation$default,
    home_team = pbp_json$homeTeam$abbrev,
    home_team_id = pbp_json$homeTeam$id,
    away_team = pbp_json$awayTeam$abbrev,
    away_team_id = pbp_json$awayTeam$id,
    home_team_place_name = pbp_json$homeTeam$placeName$default
  )
}

extract_game_rosters_api <- function(pbp_json) {
  message("extracting rosters (api)")
  pbp_json$rosterSpots |>
    tibble::tibble() |>
    dplyr::transmute(
      game_id = pbp_json$id,
      api_id = playerId,
      sweater_number = sweaterNumber,
      position_category = ifelse(positionCode |> stringr::str_detect("[CLR]"), "F", positionCode),
      position = positionCode,
      team_id = teamId
    ) |>
    dplyr::left_join(
      tibble::tibble(
        team_id = c(pbp_json$homeTeam$id, pbp_json$awayTeam$id),
        venue = c("home", "away"),
        team = c(pbp_json$homeTeam$abbrev, pbp_json$awayTeam$abbrev)
      ),
      by = dplyr::join_by(team_id)
    ) |>
    dplyr::select(
      game_id,
      api_id,
      venue,
      team,
      sweater_number,
      position_category,
      position
    )
}

extract_game_shifts_api <- function(shifts_json) {
  message("extracting shifts (api)")
  data <-
    shifts_json |>
    purrr::pluck("data") |>
    tibble::tibble()
  if (nrow(data) > 0) {
    data |>
      dplyr::filter(typeCode == 517) |>
      dplyr::transmute(
        game_id = gameId,
        game_period = period,
        event_player_1 = playerId,
        shift_start =
          purrr::map2_int(
            startTime,
            game_period,
            function(t, p) {
              t |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add((p - 1) * 1200)
            }
          ),
        shift_end =
          purrr::map2_int(
            endTime,
            game_period,
            function(t, p) {
              t |>
                stringr::str_split(":") |>
                purrr::flatten_chr() |>
                as.integer() |>
                magrittr::multiply_by(c(60, 1)) |>
                sum() |>
                magrittr::add((p - 1) * 1200)
            }
          )
      ) |>
      tryCatch(
        error = function(e) {
          message(e$message)
          tibble::tibble()
        }
      )
  } else {
    tibble::tibble()
  }
}

extract_pbp_api <- function(pbp_json) {
  message("extracting play-by-play (api)")
  pbp_json$plays |>
    dplyr::select(-typeCode) |>
    tidyr::unnest(cols = c(periodDescriptor, details)) |>
    dplyr::arrange(sortOrder) |>
    dplyr::transmute(
      game_id = pbp_json$id,
      game_period = number,
      sort_order = sortOrder,
      game_seconds =
        purrr::map2_int(
          timeInPeriod,
          game_period,
          function(t, p) {
            t |>
              stringr::str_split(":") |>
              purrr::flatten_chr() |>
              as.integer() |>
              magrittr::multiply_by(c(60, 1)) |>
              sum() |>
              magrittr::add((p - 1) * 1200)
          }
        ),
      game_strength_state = situationCode,
      home_team_def_zone = homeTeamDefendingSide |> tryCatch(error = function(e) NA_character_),
      event_team = eventOwnerTeamId,
      event_type = typeDescKey,
      event_type_detail =
        dplyr::case_when(
          !is.na(shotType) ~ shotType,
          !is.na(descKey) ~ descKey,
          T ~ NA_character_
        ) |>
        tryCatch(error = function(e) NA_integer_),
      event_player_1 =
        dplyr::case_when(
          !is.na(winningPlayerId) ~ winningPlayerId,
          !is.na(playerId) ~ playerId,
          !is.na(hittingPlayerId) ~ hittingPlayerId,
          !is.na(shootingPlayerId) ~ shootingPlayerId,
          !is.na(committedByPlayerId) ~ committedByPlayerId,
          !is.na(scoringPlayerId) ~ scoringPlayerId,
          T ~ NA_integer_
        ) |>
        tryCatch(error = function(e) NA_integer_),
      event_player_2 =
        dplyr::case_when(
          !is.na(losingPlayerId) ~ losingPlayerId,
          !is.na(hitteePlayerId) ~ hitteePlayerId,
          !is.na(blockingPlayerId) ~ blockingPlayerId,
          !is.na(drawnByPlayerId) ~ drawnByPlayerId,
          !is.na(assist1PlayerId) ~ assist1PlayerId,
          T ~ NA_integer_
        ) |>
        tryCatch(error = function(e) NA_integer_),
      event_player_3 =
        ifelse(!is.na(assist2PlayerId), assist2PlayerId, NA_integer_) |>
        tryCatch(error = function(e) NA_integer_),
      penalty_class = typeCode |>
        tryCatch(error = function(e) NA_integer_),
      penalty_in_minutes = duration |>
        tryCatch(error = function(e) NA_integer_),
      event_reason_1 = reason,
      event_reason_2 = secondaryReason,
      coords_x = xCoord,
      coords_y = yCoord,
      zone_code = zoneCode
    )
}

get_game_details_api <- function(gm_id) {
  message("getting game {gm_id} details (api)" |> glue::glue())
  pbp_json <- get_game_pbp_api(gm_id)
  shifts_json <- get_game_shifts_api(gm_id)

  list(
    meta = pbp_json |> extract_game_metadata_api(),
    rosters = pbp_json |> extract_game_rosters_api(),
    shifts = shifts_json |> extract_game_shifts_api(),
    pbp = pbp_json |> extract_pbp_api()
  )
}
