## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new(Sys.getenv("SHAREPOINT_URL"))

path_sh <- "Shared Documents/Data/naomi-raw/SSD/2022-12-05_DHIS_boundaries/areas.json"
path_sh <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), path_sh)

path_id <- "Shared Documents/Data/naomi-raw/SSD/2022-12-05_DHIS_boundaries/location_hierarchy_with-assigned-suffixes.csv"
path_id <- file.path("sites", Sys.getenv("SHAREPOINT_SITE"), path_id)

file_sh <- sharepoint$download(path_sh)
file_id <- sharepoint$download(path_id)

ids <- read_csv(file_id)
sh <- read_sf(file_sh)

#' NOTES (6 December 2022; at UNAIDS workshop):
#' * District list has 80 counties; shape file only has 79 counties.
#' * Akoka County (SSD_2_15) is missing from the boundaries file
#' * Discussed with SSD team at workshop.
#'   - They are still waiting for official boundaries from statistics bureau.
#'   - DHIS only has data for the 79 districts
#'   - ACTION: use only 79 districts for now.

st_is_valid(sh)

sh <- st_make_valid(sh)

#' Add national (level 0) boundary

sh0 <- sh %>%
  filter(area_level == 1) %>%
  group_by(area_id = "SSD_0_1", area_name = "South Sudan", area_level = 0) %>%
  summarise(.groups = "drop")

sh$area_level <- as.integer(sh$area_level)
sh <- bind_rows(sh0, sh)

areas <- ids %>%
  left_join(
    select(ids, parent_area_id_ADR = area_id_ADR, parent_area_id = area_id),
    by = "parent_area_id_ADR"
  ) %>%
  mutate(
    area_name = area_name %>%
      sub(" State", "", .) %>%
      sub(" County", "", .),
    area_level_label = area_level %>%
      recode(`0` = "Country", `1` = "State", `2` = "County"),
    spectrum_level = as.logical(area_level == 0),
    epp_level = as.logical(area_level == 0),    
    naomi_level = as.logical(area_level == 2),
    pepfar_psnu_level = as.logical(area_level == 2)
  ) %>%
  left_join(
    sh %>%
      select(area_id_ADR = area_id),
    by = "area_id_ADR"
  ) %>%
  mutate(
    area_sort_order = row_number(),
    spectrum_region_code = 0,
    center = st_centroid(geometry),
    center_x = do.call(rbind, center)[,1],
    center_y = do.call(rbind, center)[,2],
    display = TRUE
  ) %>%
  st_as_sf() %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code, area_level_label, display, spectrum_level, epp_level, naomi_level, pepfar_psnu_level) %>%
  st_make_valid()

areas <- filter(areas, !st_is_empty(areas))

st_write(areas,"ssd_areas.geojson", delete_dsn = TRUE)
