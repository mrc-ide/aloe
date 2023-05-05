# Create example internal dataset - Malawi admin 1

# Get sf
load("data/mwi.rda")

# Create mock data set
set.seed(2810)
df_mwi <- mwi |>
  sf::st_drop_geometry() |>
  dplyr::select(COUNTRY, ID_0, NAME_1) |>
  dplyr::mutate(pop = round(runif(dplyr::n(), 100000, 1000000))) |>
  tidyr::expand_grid(itn = 0:1, irs = 0:1) |>
  dplyr::group_by(COUNTRY, ID_0, NAME_1) |>
  dplyr::mutate(j = 1:dplyr::n(),
                strata = sample(1:4, 1)
                )|>
  dplyr::mutate(current = ifelse(j == sample(1:dplyr::n(), 1, prob = itn + 0.1), 1, 0)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    i = match(NAME_1, unique(NAME_1)),
    cases = round(pop * 0.01 * runif(dplyr::n(), 0.9, 1.1) *
      (1 - runif(dplyr::n(), 0.7, 0.9) * itn) *
      (1 - runif(dplyr::n(), 0.7, 0.9) * irs)),
    deaths = round(cases * runif(dplyr::n(), 0.002, 0.003)),
    cost = round((5 * pop * 0.5 * itn) +
                   (6 * pop * 0.5 * irs))
  )

usethis::use_data(df_mwi, overwrite = TRUE)

# Create example internal dataset - Sudan admin 2

# Get sf
load("data/sdn2.rda")

# Create mock data set
set.seed(2810)
df_sdn2 <- sdn2 |>
  sf::st_drop_geometry() |>
  dplyr::select(country, name_1, name_2, unique_id) |>
  dplyr::mutate(pop = round(runif(dplyr::n(), 100000, 1000000))) |>
  tidyr::expand_grid(itn = 0:1, irs = 0:1) |>
  dplyr::group_by(country, name_1, name_2, unique_id) |>
  dplyr::mutate(j = 1:dplyr::n(),
                strata = sample(1:4, 1)) |>
  dplyr::mutate(current = ifelse(j == sample(1:dplyr::n(), 1, prob = itn + irs + 0.1), 1, 0)) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    i = match(unique_id, unique(unique_id)),
    cases = round(pop * 0.01 * runif(dplyr::n(), 0.9, 1.1) *
                    (1 - runif(dplyr::n(), 0.7, 0.9) * itn) *
                    (1 - runif(dplyr::n(), 0.7, 0.9) * irs)),
    deaths = round(cases * runif(dplyr::n(), 0.002, 0.003)),
    cost = round((5 * pop * 0.5 * itn) +
                   (6 * pop * 0.5 * irs)),
    strata = sample(1:3, dplyr::n(), replace = TRUE)
  )

usethis::use_data(df_sdn2, overwrite = TRUE)
