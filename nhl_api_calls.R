get_rosters <- function(gm_id) {
  # I like to include messages so I can track progress, you can comment this
  # out to make the code run faster if you're looping through API calls though
  message(gm_id)

  boxscore_json <-
    "https://api-web.nhle.com/v1/gamecenter/{gm_id}/boxscore" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  tidyr::expand_grid(
    game_id = gm_id,
    venue = c("home", "away"),
    pos = c("forwards", "defense", "goalies")
  ) |>
  dplyr::mutate(
    roster =
      purrr::map2(
        venue,
        pos,
        function(v, p) {
          boxscore_json |>
            purrr::pluck("playerByGameStats", "{v}Team" |> glue::glue(), p) |>
            tibble::as_tibble() |>
            dplyr::transmute(
              api_id = playerId,
              sweater_number = sweaterNumber,
              position = position #ifelse(p == "forwards", "F", position)
            )
        }
      )
  ) |>
  dplyr::select(-pos) |>
  tidyr::unnest(roster)
}

get_player_bio <- function(player_api_id) {
  message(player_api_id)

  json <-
    "https://api-web.nhle.com/v1/player/{player_api_id}/landing" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  tibble::tibble(
    name =
      "{json$firstName$default} {json$lastName$default}" |>
      glue::glue(),
    handedness = json$shootsCatches,
    height_in = json$heightInInches,
    height_cm = json$heightInCentimeters,
    weight_lb = json$weightInPounds,
    weight_kg = json$weightInKilograms,
    bmi = weight_kg / ((height_cm/100)**2),
    dob = as.Date(json$birthDate)
  )
}

# The full play-by-play includes a lot of extra info, such as rosters, venues,
# etc. you can drill down into it more using purrr::pluck() to extract nested
# data in tabular form.  To get just the play-by-play detail, use
# purrr::pluck("plays") on the output of the httr::GET() call

# I've noticed that some older games the API doesn't contain the complete pbp,
# but for most of the most recent seasons, I think it's pretty complete and a
# a lot easier to use than parsing the HTML reports

get_full_play_by_play <- function(gm_id) {
  "https://api-web.nhle.com/v1/gamecenter/{gm_id}/play-by-play" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()
}

# Recent seasons also have an api call for shifts, but I'm not sure how far
# back these go.  Type code might refer to game strength, but I haven't tried
# to figure that out yet

get_shifts <- function(gm_id) {
  "https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId={gm_id}" |>
    glue::glue() |>
    httr::GET() |>
    httr::content(type = "text", encoding = "UTF-8") |>
    jsonlite::fromJSON() |>
    purrr::pluck("data") |>
    tibble::tibble()
}
