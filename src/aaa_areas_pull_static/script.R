iso3_c <- iso3

sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))
folder <- sharepoint$folder(site = Sys.getenv("SHAREPOINT_SITE"), path = "Shared Documents/Data/naomi-raw/2023_areas")
path <- folder$list()$name
path <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), "Shared Documents/Data/naomi-raw/2023_areas", path)
areas_zip <- sharepoint_download(sharepoint_url = Sys.getenv("SHAREPOINT_URL"), sharepoint_path = path)

tmpd <- tempfile()
on.exit(unlink(tmpd))
utils::unzip(areas_zip, exdir = tmpd)

areas_path <- list.files(file.path(tmpd, "2023_areas"), pattern = tolower(iso3_c), full.names = T)
areas <- read_sf(areas_path)

st_write(areas, "naomi_areas.geojson", delete_dsn = TRUE)