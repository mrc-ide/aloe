# Create example internal dataset

# Get sf
load("data/mwi.rda")

# Create mock data set
set.seed(2810)
df <- mwi |>
  sf::st_drop_geometry() |>
  dplyr::select(COUNTRY, ID_0, NAME_1) |>
  dplyr::mutate(pop = round(runif(dplyr::n(), 1000, 10000))) |>
  tidyr::expand_grid(itn = 0:1) |>
  dplyr::mutate(
    cases_averted = round(runif(dplyr::n(), 0, 1) * itn * pop),
    deaths_averted = round(runif(dplyr::n(), 0, 0.001) * itn * pop),
    cost = round(5 * pop * 0.5 * itn))

usethis::use_data(mwi, df, internal = TRUE)
