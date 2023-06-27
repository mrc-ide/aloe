# Create example internal dataset - Malawi admin 1
set.seed(1)

# Get sf
mwi <- readRDS("data-raw/mwi.rds")

generate_data <- function(spatial, spatial_id,
                          interventions = c("itn", "smc"),
                          coverages = list(itn = c(0, 0.5, 1), smc = c(0, 0.9)),
                          bau_coverages = list(itn = 0.47, smc = 0.8),
                          years = 2020:2030,
                          strata = 1:4){
  spatial$spatial_id <- spatial[[spatial_id]]
  bank_list <- c(
    list(
      spatial_id = spatial$spatial_id,
      year = years,
      scenario = "bank"
    ),
    coverages
  )
  bank <- expand.grid(bank_list)

  bau_list <- c(
    list(
      spatial_id = spatial$spatial_id,
      year = years,
      scenario = "bau"
    ),
    bau_coverages
  )
  bau <- expand.grid(bau_list)

  # Subset smc to half the coutry
  if("smc" %in% interventions){
    ct <- sf::st_coordinates(sf::st_centroid(spatial))
    no_smc_units <- spatial$spatial_id[ct[,2] > mean(ct[,2])]
    bank <- bank |>
      dplyr::filter(!(.data$spatial_id %in% no_smc_units  & .data$smc  > 0))

    no_smc_units <- unique(c(no_smc_units, sample(spatial$spatial_id, 10)))
    bau <- bau |>
      dplyr::mutate(smc = ifelse(.data$spatial_id %in% no_smc_units, 0, .data$smc))
  }

  data <- dplyr::bind_rows(bau, bank)

  strata_df <- data.frame(
    spatial_id = spatial$spatial_id
  ) |>
    dplyr::mutate(strata = sample(strata, dplyr::n(), replace = TRUE))
  data <- dplyr::left_join(data, strata_df, by = "spatial_id")

  scale = ifelse(data$year > mean(years), 1, 0)
  data$inc <- 0.1 + 0.00001 * data$year - 0.09 * scale * (rowSums(data[,interventions]) / max(rowSums(data[,interventions]))) + rnorm(nrow(data), 0, 0.01)

  colnames(data)[colnames(data) == "spatial_id"] <- spatial_id

  return(data)
}

df_mwi <- generate_data(mwi, "NAME_1")

stratification_colours <- c("#33a52a", "#b7de8d", "#fe7e00", "#d21e24")
names(stratification_colours) <- paste(1:4)

intervention_colours <- c("#855fce","#5ebe8b","#c65fb4","#c55a7f","#468045", "#7680c9")
names(intervention_colours)[1:3] <- c("itn", "smc", "irs")

intervention_mix_colours <- c("#D9D9D9", "#8DD3C7", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462",
                              "#B3DE69", "#FCCDE5",  "#BC80BD", "#CCEBC5", "#FFED6F", "#FFFFB3")

impact_colours <- rev(RColorBrewer::brewer.pal(10, "PiYG"))

usethis::use_data(intervention_colours,
                  intervention_mix_colours,
                  stratification_colours,
                  mwi,
                  df_mwi,
                  impact_colours,
                  overwrite = TRUE,
                  internal = TRUE)
