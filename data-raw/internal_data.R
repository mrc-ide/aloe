# Create example internal dataset - Malawi admin 1

# Get sf
load("data/mwi.rda")

# Create mock data set
set.seed(2810)
df_mwi <- mwi |>
  sf::st_drop_geometry() |>
  dplyr::select(COUNTRY, ID_0, NAME_1) |>
  dplyr::mutate(pop = round(runif(dplyr::n(), 1000, 10000))) |>
  tidyr::expand_grid(itn = 0:1, irs = 0:1) |>
  dplyr::group_by(COUNTRY, ID_0, NAME_1) |>
  dplyr::mutate(j = 1:dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    i = match(NAME_1, unique(NAME_1)),
    cases_averted = round(runif(dplyr::n(), 0, 1) * ((itn + irs) / 2) * pop),
    deaths_averted = round(runif(dplyr::n(), 0, 0.001) * ((itn + irs) / 2) * pop),
    cost = round((5 * pop * 0.5 * itn) +
                   (6 * pop * 0.5 * irs)))

usethis::use_data(mwi, df_mwi, overwrite = TRUE)

# Create example internal dataset - Sudan admin 2

# Get sf
load("data/sdn2.rda")

# Create mock data set
set.seed(2810)
df_sdn2 <- sdn2 |>
  sf::st_drop_geometry() |>
  dplyr::select(COUNTRY, ID_0, NAME_1, NAME_2, unique_id) |>
  dplyr::mutate(pop = round(runif(dplyr::n(), 1000, 10000))) |>
  tidyr::expand_grid(itn = 0:1, irs = 0:1) |>
  dplyr::group_by(COUNTRY, ID_0, NAME_1, NAME_2, unique_id) |>
  dplyr::mutate(j = 1:dplyr::n()) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    i = match(unique_id, unique(unique_id)),
    cases_averted = round(runif(dplyr::n(), 0, 1) * ((itn + irs) / 2) * pop),
    deaths_averted = round(runif(dplyr::n(), 0, 0.001) * ((itn + irs) / 2) * pop),
    cost = round((5 * pop * 0.5 * itn) +
                   (6 * pop * 0.5 * irs)))

usethis::use_data(df_sdn2, overwrite = TRUE)
