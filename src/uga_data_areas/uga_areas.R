#' ## Uganda (UGA)
#' Source: Uganda Bureau of Statistics
#' Levels:
#'   * 1: Region (10)
#'   * 2: Sub-region (15)
#'   * 2: District (146)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural: Yes
#' PEPFAR PSNU: District (level 2)
dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"

urls <- list(ubos2022 = "UGA/2022-11-11%20UBOS%20146%20districts%202022/uganda_districts.zip",
             uga_2022_area_id  = "UGA/2022-11-11%20UBOS%20146%20districts%202022/uga_2023_area_id.csv",
             ug_lakes = "UGA/2020-04-27/Ug_lakes.zip"
             ) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)


files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

ubos2022 <- read_sf_zip(files$ubos2022) %>% st_transform(., crs = 4326)
heirarchy <- read_csv(files$uga_2022_area_id)

uga_2022 <- ubos2022 %>%
  select(area_name3 = District) %>%
  mutate(area_name3 = str_to_sentence(area_name3), 
         area_name3 = recode(area_name3, 
                             "Kassnda" = "Kassanda", 
                             "Namutunmba" = "Namutumba",
                             "Masaka city" = "Masaka City")) %>%
  left_join(heirarchy %>%
              mutate(area_name3 = recode(area_name3, 
                                         "AruaCity" = "Arua city", 
                                         "GuluCity" = "Gulu city", 
                                         "MbararaCity" = "Mbarara city", 
                                         "HoimaCity" = "Hoima city", 
                                         "JinjaCity" = "Jinja city", 
                                         "MbaleCity" = "Mbale city", 
                                         "LiraCity" = "Lira city",
                                         "SorotiCity" = "Soroti city", 
                                         "FortPortalCity" = "Fort portal city", 
                                         "Madi Okollo" = "Madi okollo"))) %>%
  mutate(area_name3 = recode(area_name3, 
         "Arua city" = "Arua City", 
         "Gulu city" = "Gulu City", 
          "Mbarara city" = "Mbarara City", 
          "Hoima city" = "Hoima City", 
          "Jinja city" = "Jinja City", 
          "Mbale city" = "Mbale City", 
          "Lira city" = "Lira City",
          "Soroti city" = "Soroti City", 
          "Fort portal city" = "Fort portal City"))

filter(uga_2022, is.na(area_id3))

object_size(uga_2022)

uga_simple2022 <- ms_simplify(uga_2022, keep = 0.1) %>%
  st_make_valid()

object_size(uga_simple2022)

st_is_valid(uga_simple2022)


# Plot new districts 
new2022 <- uga_simple2022 %>%
  filter(nchar(area_id3) == 11)

ggplot() +
  geom_sf(data = new2022, aes(fill = area_name2)) +
  geom_sf_text(data = new2022, aes(label = area_name3), colour = "black", 
               size = 3)

#' # Uganda lakes shapefile
#' * Sent by Jotham Mubangizi on 27 April 2020; unknown source
ug_lakes <- read_sf_zip(files$ug_lakes)
uga_lakes <- st_transform(ug_lakes, 4326)

#' Remove lakes
uga_all_lakes <- st_combine(uga_lakes) %>%
  st_make_valid() %>%
  st_collection_extract() %>%
  ms_simplify(0.25) 

uga_clipped <- st_difference(uga_simple2022, uga_all_lakes) %>%
  st_collection_extract()

ggplot(uga_all_lakes) +
  geom_sf(color = "lightblue4", fill = "lightblue") +
  ggtitle("All lakes removed")

ggplot(uga_clipped) +
  geom_sf(fill = "grey70")

uga_clipped %>%
  st_union() %>%
  ggplot() +
  geom_sf(fill = "grey70")


#' Recode area hierarchy
uga_wide <- uga_clipped %>%
  mutate(
    spectrum_region_code = 0, 
    area_id0 = "UGA",
  ) %>%
  select(area_id0,
         area_name0,
         area_id1,
         area_name1,
         area_id2,
         area_name2,
         area_id3,
         area_name3,
         spectrum_region_code) %>%
  rename_all(~sub("area\\_", "", .))


uga_long <- gather_areas(uga_wide)

uga_areas <- uga_long %>%
  arrange(area_level, area_name) %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "AIS region",
                  `2` = "Sub-region",
                  `3` = "District"),
         display = TRUE,
         spectrum_level = area_level == 0,
         epp_level = area_level == 0,
         naomi_level = area_level == 3,
         pepfar_psnu_level = area_level == 3)


#' Save boundaries
sf::write_sf(uga_areas, "uga_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(uga_areas)

ggsave("uga_area_hierarchy.png", hierarchy_plot, h = 6, w = 12)

# Plot new districts 
new <- ggplot() +
  geom_sf(data = uga_long %>% filter(area_level == 2)) +
  geom_sf(data = new2022, aes(fill = area_name2)) +
  geom_sf_text(data = new2022, aes(label = area_name3), colour = "black", 
               size = 3) +
  naomi::th_map()
  
ggsave("uga_new_districts_2023.png", new, h = 6, w = 8)

# Add in brdieg between Masaka and Kalangala
masaka <- filter(uga_areas, area_name == "Masaka")$geometry
kalangala <- filter(uga_areas, area_name == "Kalangala")$geometry

bridgesf <- list(rbind(c(32.075, -0.252), c(32.075, -0.255), c(32.018, -0.255), c(32.018, -0.252), c(32.075, -0.252)))%>%
  st_polygon() %>%
  st_sfc(crs = 4326)

bridgesf <- st_difference(bridgesf, masaka)

kalangala_bridge <- st_union(bridgesf, kalangala)

ggplot() + geom_sf(data = masaka) + geom_sf(data = kalangala_bridge)

uga_areas_kalangala_bridge <- uga_areas
uga_areas_kalangala_bridge$geometry[uga_areas_kalangala_bridge$area_name == "Kalangala"] <- kalangala_bridge

sf::st_write(uga_areas_kalangala_bridge, "uga_areas_kalangala-masaka-bridge.geojson", delete_dsn = TRUE)


dev.off()

