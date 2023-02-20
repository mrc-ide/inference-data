#' ## Ghana (GHA)
#' Source: National Malaria Control Programme
#'   * 1: Region (16)
#'   * 2: District (261)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: District (level 2)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(raw_2021 = "GHA/2020-11-17/gha_adm2.zip",
             id_map_2021 = "GHA/2021-01-06/gha_area_id_map_2021.csv",
             raw_2022 = "GHA/2022-12-14-261-boundaries/Ghana%20Shape%20file%202022.zip",
             id_map_2022 = "GHA/2022-12-14-261-boundaries/2022_12%20Ghana%20Hierarchy%20Maps%20wide.csv"
             
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

raw2021 <- read_sf_zip(files$raw_2021)

gha_simple2021 <- ms_simplify(raw2021, keep = 0.03)

pryr::object_size(raw)
pryr::object_size(gha_simple2021)

tmpd <- tempfile()
utils::unzip(files$raw_2022, exdir = tmpd)

gha2022 <- read_sf(file.path(tmpd, "Ghana_SHapefiles_2017.shp")) %>%
  st_transform(3857)

guan <- read_sf(file.path(tmpd, "Guan_new.shp")) %>%
  select(area_name = adm2) %>%
  st_transform(3857)

hohoe <- read_sf(file.path(tmpd, "Hohoe_new.shp")) %>%
  select(area_name = adm2) %>%
  st_transform(3857)

p <- ggplot() +
  geom_sf(data = gha_simple2021, fill = NA, color = "black") +
  geom_sf(data = gha2022, fill = NA, color = "red") +
  geom_sf(data = guan, fill = "red", alpha = 0.5) +
  geom_sf(data = hohoe, fill = "grey", alpha = 0.5)+
  ggtitle("GHA 2021 260 district boundaries (black) \nvs. 2022 261 boundaries (red)",
          subtitle = "Grey = New Hohoe\nRed = New Guan")

ggsave("check/Ghana-2021-2022-districts.png", p, h = 6, w = 6)

regions2021 <- gha_simple2021 %>%
  group_by(area_id1) %>% summarize(geometry = st_union(geometry))

regions2022 <- gha2022 %>%
  group_by(New_Region) %>% summarize(geometry = st_union(geometry))

p <- ggplot() +
  geom_sf(data = regions2021, fill = NA, color = "black") +
  geom_sf(data = regions2022, fill = NA, color = "red") +
  ggtitle("GHA 2021 regions (black) \nvs. 2022 regions (red)")

ggsave("check/Ghana-2021-2022-regions.png", p, h = 6, w = 6)

id_map <- read_csv(files$id_map_2022)

wide2022 <- gha2022 %>%
  filter(District != "Hohoe Muni") %>%
  select(area_name = District) %>%
  rbind(guan, hohoe) %>%
  left_join(id_map, by = c("area_name" = "District in MoH 2022")) %>%
  select(area_name0, area_id0, area_name1, area_id1, area_name2, area_id2)


#' Replace old area IDs with 2021 area IDs
gha_long <- wide2022 %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

# Overlaps in district boundaries - messy regional boundaries
gha_clean <- gha_long %>%
  ms_simplify(., keep = 0.1) %>%
  st_transform(3857) %>%
  st_snap(., ., tolerance = 50) %>%
  sf::st_make_valid() %>%
  st_transform(4326)

p <- ggplot() +
  geom_sf(data = gha_long %>% filter(area_level == 1), fill = NA, color = "black") +
  geom_sf(data = gha_clean %>% filter(area_level == 1), fill = NA, color = "red") +
  ggtitle("GHA 2022 regions: raw (black) vs. snapped (red)")

ggsave("check/Ghana-2022-snapped-regions.png", p, h = 6, w = 6)


gha_clean %>% group_by(area_level) %>%
  summarise(n = n())

gha_areas <- gha_clean %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "District"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)

#' Save boundaries
sf::write_sf(gha_areas, "gha_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(gha_areas)

ggsave("gha_area_hierarchy.png", hierarchy_plot, h = 4, w = 8)
