#' ## Eritrea (ERI)
#' Source:
#'   * 1: Zobas (6)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "ERI/2022-12-06%20boundaries/Eritrea.zip",
             id_map = "ERI/2022-12-06%20boundaries/eri-area-id.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
raw <- read_sf_zip(files$raw)

raw_long <- raw %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(raw_long)

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

raw_2022 <- raw_long %>%
  select(area_id) %>%
  left_join(id_map) %>%
  select(area_id = area_id_2023, area_name, area_level) %>%
  mutate(parent_area_id = if_else(area_id == "ERI", NA_character_, "ERI"), 
         spectrum_region_code = 0)


#' Create boundaries file
eri_areas <- raw_2022 %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Zoba"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(eri_areas, "eri_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(eri_areas)
ggsave("check/eri-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
