# ID's de todos os jogos regulares do Houston Rockets (HOU) 19/20 --------------

## Parametros de Consulta
season_state <- list("All Seasons" = 'all', "Summer League" = '15',
                     "Preseason" = '1', "Regular Season" = '2',
                     "Post Season" = '4', "Play In" = '5')
season_year <- '2019'
opponents <- 'all'

## Web Scraping dos ID's (schedules)
u <- 'https://www.nba.com/rockets/ajax/schedule/overview'
query <- list(
  "season_year" = season_year,
  "season_state" = season_state[['Regular Season']],
  "opponents[]" = "all"
)

r <- httr::GET(u, query = query)

schedules <- r |>
  httr::content() |>
  purrr::pluck(3, 'layout') |>
  rvest::read_html() |>
  rvest::html_elements(xpath = "//li") |>
  rvest::html_attr("id")

# Obtenção dos Dados Jogada a Jogada (Play-by-Play - PBP) ----------------------

## Barra de Progresso
progressr::handlers(
  progressr::handler_progress(
    format   = ":spin :current/:total (:message) [:bar] :percent em :elapsed ETA: :eta",
    width    = 100,
    complete = "+"
  )
)

## Local Env para o Progressr
progressr::with_progress({

  p <- progressr::progressor(steps = length(schedules))

  ## Processamento Paralelo
  future::plan(future::multisession, workers = 5)
  furrr::future_walk2(season_year, schedules, \(season, schedule) {

    ## Definição do EndPoint
    u <- glue::glue("http://data.nba.net/v2015/json/mobile_teams/nba/{season}/scores/pbp/{schedule}_full_pbp.json")

    ## Obtenção do JSON do PBP
    ## Só vão ser printadas no final do Future...
    tryCatch(
      pbp <- jsonlite::fromJSON(txt = u),
      warning = function(war) {
        message(glue::glue("WARNING: URL não encontrada no jogo de ID {schedule}"))
        next
      },
      error = function(err) message("ERROR: {err}... Pulando para o próximo jogo"))

    ## Escrita dos dados localmente
    filename <- pbp |>
      purrr::pluck('g', 3) |>
      stringr::str_replace('/', '_')

    fs::dir_create('raw_data')

    pbp |>
      purrr::pluck('g', 'pd', 'pla') |>
      furrr::future_imap_dfr(\(d, i) dplyr::mutate(d, qrt = glue::glue("{i}st"), .before = 1)) |>
      dplyr::mutate(game_id = schedule, .before = 2) |>
      readr::write_csv(glue::glue("raw_data/{filename}.csv"))

    p(glue::glue("Jogo {filename} Importado"))
  })
})
