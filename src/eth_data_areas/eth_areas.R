#' ## Ethiopia
#' Source: Ethiopia Central Statistics Agency
#' Levels:
#'   * 1: Region (14)
#'   * 2: Zone (143)

dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

#' ## Read in files from SharePoint

naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/naomi-raw"

urls <- list(boundaries2023 = "ETH/2023-11-24-change-from-164-to-143 zones/2023-03-08-eth_areas.geojson",
             zones2024 = "ETH/2023-11-24-change-from-164-to-143 zones/Ethiopia_5_Zones_2023_Nov.zip",
             regions2024 = "ETH/2023-11-24-change-from-164-to-143 zones/Ethiopia_4_Regions_2023_Nov.zip"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) 


files <- lapply(urls, sharepoint$download)

 
areas_2023 <- read_sf(files$boundaries2023)

areas_2023 %>%
  group_by(area_level_label) %>%
  st_drop_geometry() %>%
  summarise(n = n())

areaswide_2023 <- naomi::spread_areas(areas_2023)

zones2024 <- naomi.utils::read_sf_zip(files$zones2024)
regions2024 <- naomi.utils::read_sf_zip(files$regions2024)

zones2023 <- areas_2023 %>% filter(area_level == 2)
regions2023 <- areas_2023 %>% filter(area_level == 1)

zones <- ggplot() +
  geom_sf(data = regions2023, 
          fill = NA, colour = "black") +
  geom_sf(data = regions2024, 
          fill = NA, colour = alpha("red", 0.6)) +
  th_map() +
  ggtitle("Ethiopia 2023 Boundaries: Change from 13 to 14 regions", 
          subtitle = "Black: 13 region boundaries from 2022 \nRed: 14 region boudnaries from 2023")

setdiff(zones2023$area_name, zones2024$name)
setdiff(zones2024$name, zones2023$area_name)

regions <- ggplot() +
  geom_sf(data = zones2023, 
          fill = NA, colour = "black") +
  geom_sf(data = zones2024, 
          fill = NA, colour = alpha("red", 0.6)) +
  th_map() +
  ggtitle("Ethiopia 2023 Boundaries: Change from 164 to 143 zones", 
          subtitle = "Black: 164 zone boundaries from 2022 \nRed: 143 zone boundaries from 2023")


boundary_change <- plot_grid(zones, regions)

boundary_change

ggsave("check/eth_2024_boundary_change.png", boundary_change, h = 6, w = 12)

# Decision: 
#  - Keep Regional area IDs as these have not changed beyond the SNNPR
#    split to South and Central Ethiopia 
#  - Generate new area IDs as the change from 164 to 143 zone zone boundaries
#    looks different to the file from previous year, when comparing both zones 
#    that have merged and those that have remained unchanged

object_size(zones2024)
#' Simplify boundaries to reduce file size (2.42 Mb)
zones2024_simple <- ms_simplify(zones2024, keep = 0.20) %>%
  st_make_valid()
pryr::object_size(zones2024_simple)

# Set seed to ensure random letter suffix is consistent
set.seed(42)


# Fix GEOMETRYCOLLECTION issue identified when trying to run extract worldpop
# data with these boundaries
st_geometry_type(zones2024_simple) %>% as.data.frame(.) %>% distinct(.)

# Bole zone
filter(zones2024_simple ,st_is(geometry, "GEOMETRYCOLLECTION"))

zones2024_clean <- st_collection_extract(zones2024_simple, "POLYGON")
  
st_geometry_type(zones2024_clean) %>% as.data.frame(.) %>% distinct(.)

eth2024_wide <- zones2024_clean %>%
  select(area_name = name, area_name1 = level4name, area_name2 = level5name) %>%
  left_join(regions2023 %>% select(area_id, area_name1 = area_name)
            %>% st_drop_geometry()) %>%
  # Get old area IDs for regions and create new IDs for SNNPR split
  mutate(
    area_id1 = case_when(
      area_name1 == "Central Ethiopia" ~"ETH_1_24gf", 
      area_name1 == "South Ethiopia" ~"ETH_1_25lm", 
      TRUE ~ area_id), area_id = NULL, 
    area_id1 = str_replace(area_id1, "ETH_1_", "ETH_01_")) %>%
  # Create new area IDs for zone
  arrange(area_id1, area_name) %>%
  mutate(num = sprintf("%03d", row_number()), 
         suffix = paste0(sample(letters, 143, TRUE), sample(letters, 143, TRUE)), 
         area_id2 = paste0("ETH_02_", num, suffix), 
         num = NULL, suffix = NULL,
         area_name0 = "Ethiopia", area_id0 = "ETH", 
         spectrum_region_code = case_when(
           area_name1 == "Addis Ababa" ~ 10,
           area_name1 == "Afar" ~ 11,
           area_name1 == "Amhara" ~ 12,
           area_name1 == "Benishangul-Gumuz" ~ 13,
           area_name1 == "Dire Dawa" ~ 14,
           area_name1 == "Gambella" ~ 15,
           area_name1 == "Harari" ~ 16,
           area_name1 == "Oromia" ~ 17,
           area_name1 == "Central Ethiopia" ~ 18,
           area_name1 == "South Ethiopia" ~ 18,
           area_name1 == "South West Ethiopia" ~ 18,
           area_name1 == "Somali" ~ 19,
           area_name1 == "Tigray" ~ 20,
           area_name1 == "Sidama" ~ 21)) %>%
  select(area_id0, area_name0, area_id1, area_name1, area_id2, area_name2, 
         area_name, spectrum_region_code)


eth_2024_long <- eth2024_wide %>%
  rename_all(~sub("area\\_", "", .)) %>%
  gather_areas()


eth_areas2024 <- eth_2024_long %>%
  mutate(
    area_sort_order = row_number(),
    center = sf::st_point_on_surface(geometry),
    center_x = sf::st_coordinates(center)[,1],
    center_y = sf::st_coordinates(center)[,2],
    center = NULL,
    area_level_label = area_level %>%
      recode(`0` = "Country", `1` = "Region", `2` = "Zone")
  )


#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(eth_areas2024)
hierarchy_plot 
ggsave("eth_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)


#' Save boundaries
sf::st_write(eth_areas2024, "eth_areas.geojson", delete_dsn = TRUE)

while (!is.null(dev.list())) dev.off()
