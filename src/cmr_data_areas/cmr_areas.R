#' ## Cameroon (CMR)
#' Source: UNAIDS
#' Levels:
#'   * 1: Région Sanitaire (10)
#'   * 1: Région Sanitaire + Villes (12)
#'   * 2: District Sanitaire  (197)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural:
#' PEPFAR PSNU:
#' 
#' 2024 estimates update:
# * Admin 0 = National (for National HIV estimates)
# * Admin 1 = 10 régions (for PEPFAR COP planning and regional planning)
# * Admin 2 = 10 régions + 2 villes (for regional planning and city level planning 
#                                    as it also takes into account 2 major cities)
# * Admin 3 = 200 districts for district level planning

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts

## Revert back to planar geometry
sf::sf_use_s2(FALSE)
dir.create("check")

#' Authenticate SharePoint login
 sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#' 
#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"
#' 
#' 
#' #' Read in files from SharePoint
urls <- list( boundaries_2023 = "CMR/2023-10-13%20197%20to%20200%20districts%20(DHIS)/cameroon-areas.geojson",
              boundaries_2024 = "CMR/2023-10-13%20197%20to%20200%20districts%20(DHIS)/CMR%20DHIS.zip"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)



raw_2024 <- read_sf_zip_list(files$boundaries_2024) %>%
  as.data.frame() %>%
  st_as_sf()

cmr_areas_2022 <- read_sf(files$boundaries_2023)

object_size(raw_2024)

cmr_simple <- raw_2024 %>% 
  st_transform(crs = '+proj=aeqd +lat_0=53.6 +lon_0=12.7') %>%
  st_simplify(., preserveTopology = FALSE, dTolerance = 200) %>%
  as.data.frame() %>%
  st_as_sf() %>%
  sf::st_make_valid() %>%
  st_transform(4326)

cmr_2024 <- cmr_simple %>%
  filter(area_level == 2) %>%
  mutate(area_name = str_replace(area_name, "District ", "")) %>%
  select(area_name) %>%
  left_join(cmr_areas_2022 %>% select(area_name, area_id, parent_area_id) %>%
              st_drop_geometry())

cmr_2024 %>%
  filter(is.na(area_id))

compare_map_boundaries <- function(area_names, 
                                   df1, df2, plot_title) {
  
  theme <- theme(axis.text.x = element_blank(),
                 axis.text.y = element_blank(),
                 axis.ticks = element_blank(), 
                 plot.margin = margin(0,0,0,0))
  
  title  <- ggdraw() + 
    draw_label(plot_title, fontface = 'bold', x = 0, hjust = 0) +
    theme(plot.margin = margin(0,0,1,0))
  
  a <-  ggplot() + geom_sf(data = df1 %>% filter(area_name %in% area_names), 
                           aes(fill = area_name)) + theme +
    ggtitle("Boundaries 2022")
  
  
  b <-  ggplot() + geom_sf(data = df2 %>% filter(area_name %in% area_names), 
                           aes(fill = area_name)) + theme +
    ggtitle("Boundaries 2023")
  
  maps <- plot_grid(a,b, nrow = 1) 
  plot_grid(title, maps, ncol = 1, rel_heights = c(0.1, 1))
  
}


# Visualize changes
# District Ambam -> split into Ambam and Kye ossi

p1 <- compare_map_boundaries(area_names = c("Olamze", "Ambam", "Kye ossi"), 
                       df1 = cmr_areas_2022, df2 = cmr_2024, 
                       plot_title = "Ambam split into Ambam and Kye ossi")
p1


# District Kribi -> split into Kribi and Niete
p2 <- compare_map_boundaries(area_names = c("Kribi", "Niete"), 
                             df1 = cmr_areas_2022, df2 = cmr_2024, 
                             plot_title = "Kribi split into Kribi and Niete")
p2

# District Kribi -> split into Kribi and Niete
names <- c("Garoua 1", "Garoua 2", "Garoua I", "Garoua II", 
           "Gashinga", "Pitoa", "Gaschiga")

p3 <- compare_map_boundaries(area_names = names, 
                             df1 = cmr_areas_2022, df2 = cmr_2024, 
                             plot_title = "Gaschiga and Pitoa boundaries extended into Garoua I + II")
p3

# Figure out where Belel is
names <- c("Belel", "Ngaoundere Rural", "Ngaoundere Urbain")

p4 <- compare_map_boundaries(area_names = names, 
                             df1 = cmr_areas_2022, df2 = cmr_2024, 
                             plot_title = "Ngaoundere Rural split into Ngaoundere Rural and Belel")
p4


plot_grid(p1, p2, p3, p4, ncol = 1)


# Change area_id for new districts and altered boundaries
cmr_clean <- cmr_2024 %>%
  mutate(
    area_id = case_when(
      area_name == "Ambam" ~ "CMR_3_183jg", 
      area_name == "Kye ossi" ~ "CMR_3_198lm",
      area_name == "Kribi" ~ "CMR_3_181sm", 
      area_name == "Niete" ~ "CMR_3_199jv",
      area_name == "Gaschiga" ~ "CMR_3_106ag",
      area_name == "Garoua 1" ~ "CMR_3_104es", 
      area_name == "Garoua 2" ~ "CMR_3_105dj", 
      area_name == "Pitoa" ~ "CMR_3_109wk",
      area_name == "Ngaoundere Rural" ~ "CMR_3_8pk", 
      area_name == "Belel" ~ "CMR_3_200rt", 
      TRUE ~ area_id), 
    parent_area_id = case_when(
      area_name == "Kye ossi" ~ "CMR_2_9",
      area_name == "Niete" ~ "CMR_2_9",
      area_name == "Belel" ~ "CMR_2_1", 
      area_name == "Garoua 1" ~ "CMR_2_6", 
      area_name == "Garoua 2" ~ "CMR_2_6", 
      TRUE ~ parent_area_id), 
    area_level = 3)

cmr_clean %>%
  filter(is.na(area_id))

cmr_clean %>%
  filter(is.na(parent_area_id))

cmr_final <- cmr_areas_2022 %>%
  select(area_id, area_name, parent_area_id, area_level) %>%
  filter(area_level %in% 0:2) %>%
  bind_rows(cmr_clean) %>%
  mutate(area_id = str_replace(area_id, "CMR_1", "CMR_01"), 
         area_id = str_replace(area_id, "CMR_2", "CMR_02"), 
         area_id = str_replace(area_id, "CMR_3", "CMR_03"), 
         parent_area_id = str_replace(parent_area_id, "CMR_1", "CMR_01"), 
         parent_area_id = str_replace(parent_area_id, "CMR_2", "CMR_02"))

cmr_areas_2023 <- cmr_final %>%
  arrange(parent_area_id, area_id) %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         spectrum_region_code = 0,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Région",
                                   `2` = "Région + Villes",
                                   `3` = "District Sanitaire"),
         display = TRUE, 
         area_sort_order = row_number()) %>%
           select(area_id, area_name, parent_area_id, area_level, area_level_label,
                  spectrum_region_code, display, area_sort_order,
                  center_x, center_y, geometry)


hierarchy_plot <- plot_area_hierarchy_summary(cmr_areas_2023)
dir.create("check")

ggsave("check/cmr-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

while (!is.null(dev.list())) dev.off()

#' Save boundaries
sf::st_write(cmr_areas_2023, "cmr_areas.geojson", delete_dsn = TRUE)

