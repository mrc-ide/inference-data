# sessionInfo()

#' Importing data from finalised UNAIDS Naomi datasets 

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

if(version == "2022") {folder <- "2022 naomi preliminary"}
if(version == "2023") {folder <- "2023 naomi final"}

folder_path <- paste0("Shared Documents/Data/Spectrum files/", folder, "/")

outputs <- folder_path %>%
  lapply(function(x) sharepoint$folder("HIVInferenceGroup-WP", URLencode(x))) %>%
  lapply(function(x) x$files()$name) %>%
  unlist()

dir.create("outputs")

if(is_empty(grep(iso3, c("SSD", "ERI")))) {
  
  naomi_outputs <- outputs[grepl(".zip", outputs)]
  country_output <- naomi_outputs[grepl(iso3, naomi_outputs)]
  
  # Read output zip from Sharepoint
  url <- URLencode(file.path("sites/HIVInferenceGroup-WP/", folder_path, country_output))
  file <- sharepoint$download(url)
  
  # Read in naomi output zip
  out <- read_output_package(file)
  iso <- out$fit$model_options$area_scope
  
  # Stop if iso3 from output file does not match iso3 from task
  stopifnot(iso == iso3)
  
  # Add iso3 task to areas file
  areas <- out$meta_area
  areas$iso3 <- iso
  
  naomi::save_output_package(out, "naomi_output", "outputs", overwrite = TRUE)
  
} else {
  
  areas_path <- outputs[grepl(tolower(iso3), outputs)]
  url <- URLencode(file.path("sites/HIVInferenceGroup-WP/", folder_path, areas_path))
  file <- sharepoint$download(url)
  areas <- read_sf(file)
  areas$iso3 <- iso3
  
  naomi_out <- data.frame()
  write_csv(naomi_out, paste0(tempdir(), "/empty_df.csv"))
  zip::zipr("outputs/naomi_output.zip", paste0(tempdir(), "/empty_df.csv"))
}

# Save areas file
st_write(areas, "outputs/naomi_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(areas)
ggsave("outputs/hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

