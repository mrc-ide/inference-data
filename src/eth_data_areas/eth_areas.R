#' ## Ethiopia
#' Source: PEPFAR
#' Levels:
#'   * 1: Region (13)
#'   * 2: Zone (164)
#' PEPFAR PSNU: Woreda (level 3)
#' Spectrum: Region (level 1)
#' EPP: Region (level 1)
#' EPP Urban/Rural: Yes

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' ## Read in files from SharePoint


#' 2023 boundaries (164 zones)
#' Read in files from SharePoint
#'
#' Use extract of Datim org hierarchy for boundaries
#' * The list of Zones have been updated in Datim but boundaries have not
#' * Construct new boundaries using boundaries from the promoted Woredas

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw"

urls <- list(boundaries2023 = "ETH/2022-12-12 new-zones-2023/eth-datim-2022-12-08.geojson",
             idmap_2023 = "ETH/2022-12-12 new-zones-2023/eth-area-id-2023.csv",
             areas2022 = "ETH/2022-12-12 new-zones-2023/eth_areas_2022.geojson"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

boundaries <- read_sf(files$boundaries2023)
areasraw2023 <- read_csv(files$idmap_2023)
areas2022 <- read_sf(files$areas2022)


#' Construct boundaries for new Zones from promoted Woredas

new_zones <- c("Debre Markos Town", "Debre Birhan Town", "Kombolcha Town")
edit_zones <- c("East Gojam", "North Shewa (Amhara)", "South Wolo")

new_boundaries <- boundaries %>%
  filter(level == 6, name %in% new_zones) %>%
  select(name)

old_boundaries <- areas2022 %>%
  filter(area_level == 2, area_name %in% edit_zones) %>%
  select(name = area_name)

edit_boundaries <- old_boundaries %>%
  st_difference(st_union(new_boundaries))

p_new_zones <- bind_rows(
  mutate(old_boundaries, label = "0. Old Zones"),
  mutate(new_boundaries, label = "1. New Zones"),
  mutate(edit_boundaries, label = "2. Edited Zones")
) %>%
  ggplot(aes(fill = label)) +
  geom_sf() +
  facet_wrap(~label) +
  theme_void() +
  theme(panel.border = element_rect())

ggsave("check/new-zone-2023-boundaries.pdf", p_new_zones, h = 3, w = 9.5)

new_and_edit_boundaries <- bind_rows(new_boundaries, edit_boundaries) %>%
  select(area_name = name, geometry_new = geometry)

areas2023 <- areasraw2023 %>%
  left_join(
    select(areas2022, area_id),
    by = c("area_id2023" = "area_id")
  ) %>%
  left_join(new_and_edit_boundaries, by = "area_name")


stopifnot(sum(!st_is_empty(areas2023$geometry_new)) == 6)
stopifnot(sum(st_is_empty(areas2023$geometry)) == 6)

areas2023 <- areas2023 %>%
  mutate(
    geometry = st_cast(geometry, "GEOMETRY"),
    geometry = if_else(!st_is_empty(geometry_new), geometry_new, geometry),
    geometry_new = NULL
  ) %>%
  st_as_sf()

stopifnot(!st_is_empty(areas2023$geometry))
stopifnot(st_is_valid(areas2023))


#' Look for overlapping areas

overlap <- areas2023 %>%
  select(area_level, area_id2023, area_name) %>%
  st_intersection(., .) %>%
  filter(area_level == area_level.1,
         area_id2023 != area_id2023.1) %>%
  mutate(area = st_area(.))

overlap %>%
  filter(as.numeric(area) > 0.1) %>%
  arrange(-area) %>%
  print(n = Inf)

areas2023 %>%
  filter(area_name %in% c("West Hararge", "East Bale")) %>%
  ggplot(aes(fill = area_name)) +
  geom_sf(alpha = 0.4)

#' On plotting the largest of overlaps, the overlap is visually imperceptible.
#' Don't worry about this for now

#' # Simplify and clean boundaries

pryr::object_size(areas2023)

stopifnot(st_is_valid(areas2023))
stopifnot(st_geometry_type(areas2023) %in% c("POLYGON", "MULTIPOLYGON"))


spectrum_region_code <- c(
  "Addis Ababa" = 10,
  "Afar" = 11,
  "Amhara" = 12,
  "Benishangul-Gumuz" = 13,
  "Dire Dawa" = 14,
  "Gambella" = 15,
  "Harari" = 16,
  "Oromia" = 17,
  "SNNPR" = 18,
  "South West Ethiopia" = 18,
  "Somali" = 19,
  "Tigray" = 20,
  "Sidama" = 21
)

areas2023 <- areas2023 %>%
  left_join(
    areas2022 %>%
      st_drop_geometry() %>%
      select(area_id2022 = area_id, spectrum_region_code, area_sort_order),
    by = "area_id2022"
  ) %>%
  arrange(area_sort_order)

eth_areas2023 <- areas2023 %>%
  mutate(
    area_id = area_id2023,
    area_sort_order = row_number(),
    center = sf::st_point_on_surface(geometry),
    center_x = sf::st_coordinates(center)[,1],
    center_y = sf::st_coordinates(center)[,2],
    center = NULL,
    area_level_label = area_level %>%
      recode(`0` = "Country", `1` = "Region", `2` = "Zone"),
    display = TRUE,
    spectrum_level = area_level == 1,
    epp_level = area_level == NA,
    naomi_level = area_level == 2,
    pepfar_psnu_level = area_level == 2
  ) %>%
  select(all_of(names(areas2022)))

stopifnot(!duplicated(eth_areas2023$area_id))
stopifnot(!duplicated(eth_areas2023$area_sort_order))

#' Plot hierarchy
plot_area_hierarchy_summary(eth_areas2023)
# Overlapping zone boundaries -> fragments in regional + national boundaries

#' Save boundaries

sf::st_write(eth_areas2023, "eth_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(eth_areas2023)
ggsave("eth_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)



#' Datim area map

area_map_datim <- areas2023 %>%
  st_drop_geometry() %>%
  select(area_id = area_id2023, map_level = level, map_name = name, map_id = id) %>%
  mutate(map_source = "Datim")

write_csv(area_map_datim, "eth_area_map_datim_2023.csv", na = "")

while (!is.null(dev.list())) dev.off()
