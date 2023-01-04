#' ## Mali (MLI)
#' Source:
#'   * 1: Region (11)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw = "MLI/2021-03-10/Mali.zip",
             id_map = "MLI/2021-03-10/mli_area_id_map_2021.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

# Read in files from Sharepoint
raw <- read_sf_zip(files$raw) %>%
  select(area_name1 = ADM1_NAME, geometry) %>%
  mutate(area_name0 = "Mali",
         area_id0 = "MLI",
         area_name1 = str_to_sentence(area_name1),
         region_rank = dense_rank(as_factor(area_name1)),
         area_id1 = paste0("MLI_1_", str_pad(region_rank, 2, pad = "0")),
         spectrum_region_code = 0L) %>%
  select(area_id0, area_name0, area_id1, area_name1, spectrum_region_code)

# summarise geometry to regional level

mli_simple <- raw %>%
  group_by(area_name1) %>%
  summarise(geometry = st_cast(st_union(geometry))) %>%
  ms_simplify(., keep = 0.1) %>%
  st_transform(3857) %>%
  st_snap(., ., tolerance = 600) %>%
  sf::st_make_valid() %>%
  st_transform(4326)

pryr::object_size(mli_regional)

# Check that regional boundaries aggregate up to national level
mli_simple %>%
  st_union() %>%
  plot()

p_compare_boundaries <- compare_boundaries(raw, mli_simple) +
  ggtitle("Mali regional boundaries")

ggsave("check/lbr-district-boundaries-reduced.png", p_compare_boundaries, h = 6, w = 4.5)

mli_long <- mli_regional %>%
  rename_all(~sub("area\\_", "", .)) %>%
  gather_areas()

stopifnot(st_is_valid(mli_long))
stopifnot(st_geometry_type(mli_long) %in% c("POLYGON", "MULTIPOLYGON"))

#' Replace old area IDs with 2021 area IDs
id_map <- read_csv(files$id_map)

mli <- mli_long%>%
  mutate(across(c(area_id,parent_area_id),
                ~id_map$area_id_2021[match(., id_map$area_id)]))

mli %>% group_by(area_level) %>%
  summarise(n = n())

#' Create boundaries file
mli_areas <- mli %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)


#' Save boundaries
sf::st_write(mli_areas, "mli_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(mli_areas)
ggsave("check/mli-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()
