# data cleaning for NHL edge player data.  Removes duplicates and generally
# cleans things up by converting to simpler format

readr::read_csv("zonetime_all_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_all_regular_clean.csv")


readr::read_csv("zonetime_es_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_es_regular_clean.csv")

readr::read_csv("zonetime_pp_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_pp_regular_clean.csv")

readr::read_csv("zonetime_pk_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_pk_regular_clean.csv")

regular_season_games <-
  readr::read_csv("skatingdistance_all_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::pull(X3) |>
  unique() |>
  sort() |>
  purrr::map(
    function(game_date) {
      httr::GET("https://api-web.nhle.com/v1/score/{game_date}" |> glue::glue()) |>
        httr::content() |>
        purrr::pluck("games") |>
        purrr::map(
          function(game) {
            tibble::tibble(
              game_id = game$id,
              date = game$gameDate,
              home_team = game$homeTeam$abbrev,
              away_team = game$awayTeam$abbrev
            )
          }
        ) |>
        dplyr::bind_rows()

    }
  ) |>
  dplyr::bind_rows()

regular_season_games <-
  regular_season_games |>
  dplyr::mutate(date = lubridate::as_date(date))

readr::read_csv("skatingdistance_all_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(regular_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(regular_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_all_regular_clean.csv")

readr::read_csv("skatingdistance_es_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(regular_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(regular_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_es_regular_clean.csv")

readr::read_csv("skatingdistance_pp_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(regular_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(regular_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_pp_regular_clean.csv")

readr::read_csv("skatingdistance_pk_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(regular_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(regular_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_pk_regular_clean.csv")

readr::read_csv("skatingdistance_all_regular_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_all_regular_clean.csv")

readr::read_csv("skatingdistance_es_regular_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_es_regular_clean.csv")

readr::read_csv("skatingdistance_pp_regular_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_pp_regular_clean.csv")

readr::read_csv("skatingdistance_pk_regular_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_pk_regular_clean.csv")

player_toi_all <-
  readr::read_csv("skatingdistance_all_regular_clean.csv") |>
  dplyr::filter(distance != 0) |>
  dplyr::group_by(id, season) |>
  dplyr::summarise(toi = sum(toi_num), .groups = "drop")

readr::read_csv("skatingspeed_all_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X5,
    season = X6,
    max_skating_speed = X1,
    bursts_22 = X2,
    bursts_20 = X3,
    bursts_18 = X4,
    max_skating_speed_date =
      X7 |>
      stringr::str_extract("\\d+/\\d+/\\d+") |>
      lubridate::mdy(),
    max_skating_speed_opponent =
      X7 |>
      stringr::str_extract("[A-Z]{3}"),
    max_skating_speed_game_seconds =
      (
        stringr::str_extract(X7, "Period \\d") |>
          stringr::str_extract("\\d") |>
          as.numeric() |>
          magrittr::subtract(1) |>
          magrittr::multiply_by(20 * 60)
      ) + (
        stringr::str_extract(X7, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X7, "\\d+$") |>
          as.numeric()
      )
  ) |>
  dplyr::left_join(player_toi_all) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_all_regular_clean.csv") |>
      dplyr::select(
        id,
        season,
        max_skating_speed_game_id = game_id,
        max_skating_speed_date = date,
        max_skating_speed_opponent = opponent
      )
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_all_regular_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_all = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_es_regular_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_ev = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_pp_regular_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_pp = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_pk_regular_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_pk = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::transmute(
    id, season, toi,
    bursts_22,
    bursts_20,
    bursts_18,
    bursts_22_per_60 = bursts_22 / (toi / 3600),
    bursts_20_per_60 = bursts_20 / (toi / 3600),
    bursts_18_per_60 = bursts_18 / (toi / 3600),
    avg_speed_all,
    avg_speed_ev,
    avg_speed_pp,
    avg_speed_pk,
    max_skating_speed,
    max_skating_speed_date,
    max_skating_speed_game_id,
    max_skating_speed_opponent,
    max_skating_speed_game_seconds
  ) |>
  readr::write_csv("skatingspeed_all_regular_clean.csv")

readr::read_csv("shotspeed_all_regular.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X7,
    season = X8,
    max_shot_speed = X1,
    avg_shot_speed = X2,
    shots_100 = X3,
    shots_90 = X4,
    shots_80 = X5,
    shots_70 = X6,
    max_shot_speed_date =
      X9 |>
      stringr::str_extract("\\d+/\\d+/\\d+") |>
      lubridate::mdy(),
    max_shot_speed_opponent =
      X9 |>
      stringr::str_extract("[A-Z]{3}"),
    max_shot_speed_game_seconds =
      (
        stringr::str_extract(X9, "Period \\d") |>
          stringr::str_extract("\\d") |>
          as.numeric() |>
          magrittr::subtract(1) |>
          magrittr::multiply_by(20 * 60)
      ) + (
        stringr::str_extract(X9, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X9, "\\d+$") |>
          as.numeric()
      )
  ) |>
  dplyr::left_join(player_toi_all) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_all_regular_clean.csv") |>
      dplyr::select(
        id,
        season,
        max_shot_speed_game_id = game_id,
        max_shot_speed_date = date,
        max_shot_speed_opponent = opponent
      )
  ) |>
  dplyr::transmute(
    id, season, toi,
    shots_100,
    shots_90,
    shots_80,
    shots_70,
    shots_100_per_60 = shots_100 / (toi / 3600),
    shots_90_per_60 = shots_90 / (toi / 3600),
    shots_80_per_60 = shots_80 / (toi / 3600),
    shots_70_per_60 = shots_70 / (toi / 3600),
    avg_shot_speed,
    max_shot_speed,
    max_shot_speed_date,
    max_shot_speed_game_id,
    max_shot_speed_opponent,
    max_shot_speed_game_seconds
  ) |>
  readr::write_csv("shotspeed_all_regular_clean.csv")











































readr::read_csv("zonetime_all_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_all_playoffs_clean.csv")


readr::read_csv("zonetime_es_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_es_playoffs_clean.csv")

readr::read_csv("zonetime_pp_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_pp_playoffs_clean.csv")

readr::read_csv("zonetime_pk_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::rename(
    id = X1,
    season = X2,
    zone = X3,
    zonetime_perc = X4
  ) |>
  readr::write_csv("zonetime_pk_playoffs_clean.csv")

playoffs_season_games <-
  readr::read_csv("skatingdistance_all_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::pull(X3) |>
  unique() |>
  sort() |>
  purrr::map(
    function(game_date) {
      httr::GET("https://api-web.nhle.com/v1/score/{game_date}" |> glue::glue()) |>
        httr::content() |>
        purrr::pluck("games") |>
        purrr::map(
          function(game) {
            tibble::tibble(
              game_id = game$id,
              date = game$gameDate,
              home_team = game$homeTeam$abbrev,
              away_team = game$awayTeam$abbrev
            )
          }
        ) |>
        dplyr::bind_rows()

    }
  ) |>
  dplyr::bind_rows()

playoffs_season_games <-
  playoffs_season_games |>
  dplyr::mutate(date = lubridate::as_date(date))

readr::read_csv("skatingdistance_all_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_all_playoffs_clean.csv")

readr::read_csv("skatingdistance_es_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_es_playoffs_clean.csv")

readr::read_csv("skatingdistance_pp_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_pp_playoffs_clean.csv")

readr::read_csv("skatingdistance_pk_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X1,
    season = X2,
    date = X3,
    distance = X4,
    toi_num =
      (
        stringr::str_extract(X6, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X6, "\\d+$") |>
          as.numeric()
      ),
    toi_formatted = stringr::str_remove(X6, "TOI: "),
    opponent = X5 |> stringr::str_sub(start = -3),
    home_team = ifelse(stringr::str_detect(X5, "@"), opponent, NA_character_),
    away_team = ifelse(is.na(home_team), opponent, NA_character_)
  ) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "home_team"), suffix = c("", "_2")) |>
  dplyr::left_join(playoffs_season_games, by = c("date", "away_team"), suffix = c("", "_2")) |>
  dplyr::transmute(
    id,
    season,
    game_id = ifelse(is.na(game_id), game_id_2, game_id),
    date,
    team = ifelse(is.na(away_team_2), home_team_2, away_team_2),
    opponent,
    venue = ifelse(is.na(home_team), "home", "away"),
    distance,
    toi_num,
    toi_formatted
  ) |>
  readr::write_csv("skatingdistance_pk_playoffs_clean.csv")

readr::read_csv("skatingdistance_all_playoffs_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_all_playoffs_clean.csv")

readr::read_csv("skatingdistance_es_playoffs_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_es_playoffs_clean.csv")

readr::read_csv("skatingdistance_pp_playoffs_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_pp_playoffs_clean.csv")

readr::read_csv("skatingdistance_pk_playoffs_clean.csv") |>
  dplyr::mutate(avg_mph = distance / (toi_num / 3600)) |>
  readr::write_csv("skatingdistance_pk_playoffs_clean.csv")

player_toi_all_playoffs <-
  readr::read_csv("skatingdistance_all_playoffs_clean.csv") |>
  dplyr::filter(distance != 0) |>
  dplyr::group_by(id, season) |>
  dplyr::summarise(toi = sum(toi_num), .groups = "drop")

readr::read_csv("skatingspeed_all_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X5,
    season = X6,
    max_skating_speed = X1,
    bursts_22 = X2,
    bursts_20 = X3,
    bursts_18 = X4,
    max_skating_speed_date =
      X7 |>
      stringr::str_extract("\\d+/\\d+/\\d+") |>
      lubridate::mdy(),
    max_skating_speed_opponent =
      X7 |>
      stringr::str_extract("[A-Z]{3}"),
    max_skating_speed_game_seconds =
      (
        stringr::str_extract(X7, "Period \\d") |>
          stringr::str_extract("\\d") |>
          as.numeric() |>
          magrittr::subtract(1) |>
          magrittr::multiply_by(20 * 60)
      ) + (
        stringr::str_extract(X7, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X7, "\\d+$") |>
          as.numeric()
      )
  ) |>
  dplyr::left_join(player_toi_all_playoffs) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_all_playoffs_clean.csv") |>
      dplyr::select(
        id,
        season,
        max_skating_speed_game_id = game_id,
        max_skating_speed_date = date,
        max_skating_speed_opponent = opponent
      )
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_all_playoffs_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_all = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_es_playoffs_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_ev = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_pp_playoffs_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_pp = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_pk_playoffs_clean.csv") |>
      dplyr::filter(distance != 0) |>
      dplyr::group_by(id, season) |>
      dplyr::summarise(avg_speed_pk = sum(distance) / sum(toi_num / 3600), .groups = "drop")
  ) |>
  dplyr::transmute(
    id, season, toi,
    bursts_22,
    bursts_20,
    bursts_18,
    bursts_22_per_60 = bursts_22 / (toi / 3600),
    bursts_20_per_60 = bursts_20 / (toi / 3600),
    bursts_18_per_60 = bursts_18 / (toi / 3600),
    avg_speed_all,
    avg_speed_ev,
    avg_speed_pp,
    avg_speed_pk,
    max_skating_speed,
    max_skating_speed_date,
    max_skating_speed_game_id,
    max_skating_speed_opponent,
    max_skating_speed_game_seconds
  ) |>
  readr::write_csv("skatingspeed_all_playoffs_clean.csv")

readr::read_csv("shotspeed_all_playoffs.csv", col_names = F) |>
  dplyr::distinct() |>
  dplyr::transmute(
    id = X7,
    season = X8,
    max_shot_speed = X1,
    avg_shot_speed = X2,
    shots_100 = X3,
    shots_90 = X4,
    shots_80 = X5,
    shots_70 = X6,
    max_shot_speed_date =
      X9 |>
      stringr::str_extract("\\d+/\\d+/\\d+") |>
      lubridate::mdy(),
    max_shot_speed_opponent =
      X9 |>
      stringr::str_extract("[A-Z]{3}"),
    max_shot_speed_game_seconds =
      (
        stringr::str_extract(X9, "Period \\d") |>
          stringr::str_extract("\\d") |>
          as.numeric() |>
          magrittr::subtract(1) |>
          magrittr::multiply_by(20 * 60)
      ) + (
        stringr::str_extract(X9, "\\d+:") |>
          stringr::str_remove(":") |>
          as.numeric() |>
          magrittr::multiply_by(60)
      ) + (
        stringr::str_extract(X9, "\\d+$") |>
          as.numeric()
      )
  ) |>
  dplyr::left_join(player_toi_all_playoffs) |>
  dplyr::left_join(
    readr::read_csv("skatingdistance_all_playoffs_clean.csv") |>
      dplyr::select(
        id,
        season,
        max_shot_speed_game_id = game_id,
        max_shot_speed_date = date,
        max_shot_speed_opponent = opponent
      )
  ) |>
  dplyr::transmute(
    id, season, toi,
    shots_100,
    shots_90,
    shots_80,
    shots_70,
    shots_100_per_60 = shots_100 / (toi / 3600),
    shots_90_per_60 = shots_90 / (toi / 3600),
    shots_80_per_60 = shots_80 / (toi / 3600),
    shots_70_per_60 = shots_70 / (toi / 3600),
    avg_shot_speed,
    max_shot_speed,
    max_shot_speed_date,
    max_shot_speed_game_id,
    max_shot_speed_opponent,
    max_shot_speed_game_seconds
  ) |>
  readr::write_csv("shotspeed_all_playoffs_clean.csv")





