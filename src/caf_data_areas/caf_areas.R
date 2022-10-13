#' ## Central African Republic (CAF)
#' Source: MOH (received by mail from UNAIDS)
#'   * 1: Region sanitaires (7)
#'   * 2: Prefecture (17)
#'   * 3: District sanitaire (35)
#' Spectrum: 0
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "CAF/2021-02-24/CAR_boundaries.zip",
             id_map = "CAF/2021-02-24/caf_area_id_map_2021.csv"
             
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

raw <- read_sf_zip(files$raw) %>%
  select(area_name0 = ADM0_VIZ_N, area_name1 = Région_sa, area_name2 = Prefecture,
         area_name3 = District_S) %>%
  arrange(area_name1, area_name2, area_name3) %>%
  mutate(region_rank = dense_rank(as_factor(area_name1)),
         prefecture_rank = dense_rank(as_factor(area_name2)),
         area_id1 = paste0("CAF_1_", str_pad(region_rank, 2, pad = "0")),
         area_id2 = paste0("CAF_2_", str_pad(prefecture_rank, 2, pad = "0")),
         area_id3 = paste0("CAF_3_", str_pad(row_number(), 2, pad = "0")),
         spectrum_region_code = 0L,
         area_id0 = "CAF") %>%
  select(area_id0, area_name0, area_id1, area_name1, area_id2, area_name2,
         area_id3, area_name3, spectrum_region_code)

caf_long <- raw %>%
  rename_all(~sub("area\\_", "", .)) %>%
  gather_areas()

#' Simplify boundaries to reduce file size (if > 1Mb)
pryr::object_size(caf_long)


#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

caf_2021 <- caf_long %>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

caf_2021 %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
caf_areas <- caf_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region sanitaire",
                  `2` = "Prefecture",
                  `3` = "District sanitaire"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry) %>%
  st_make_valid()

stopifnot(st_is_valid(caf_areas))

#' Save boundaries
sf::st_write(caf_areas, "caf_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(caf_areas)
ggsave("caf_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)
