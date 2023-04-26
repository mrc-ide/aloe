# Script to facilitate hosting on shinyapps.io
pkgload::load_all(".")
app()
# app(spatial = sdn2, df = df_sdn2, interventions = c("itn", "irs"), spatial_id = "unique_id")
