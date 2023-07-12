#' Importing data from finalised UNAIDS Naomi datasets 

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

folder_path <- paste0("Shared Documents/Data/Spectrum files/", folder, "/")

outputs <- folder_path %>%
  lapply(function(x) sharepoint$folder("HIVInferenceGroup-WP", URLencode(x))) %>%
  lapply(function(x) x$files()$name) %>%
  unlist()

naomi_outputs <- outputs[grepl(".zip", outputs)]

# if(iso3 == "SWZ"){
#   country_output <- naomi_outputs[grepl("ESW", naomi_outputs)]
# } else {
  country_output <- naomi_outputs[grepl(iso3, naomi_outputs)]
# }

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

# Save out naomi output zip
dir.create("outputs")
naomi::save_output_package(out, "naomi_output", "outputs", overwrite = TRUE)

# Save areas file
st_write(areas, "outputs/naomi_areas.geojson", delete_dsn = TRUE)

