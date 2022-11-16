#' ## Dominican Republic (DOM)
#' Source:
#'   * 1: Region (10)
#'   * 2: Province (32)
#' Spectrum:
#' EPP:
#' EPP Urban/Rural:
#' PEPFAR PSNU:
dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw"


urls <- list(prov = "DOM/2022-11-09-initial_dom_naomi_files/provcenso2010.zip",
             datim = "DOM/2022-11-09-initial_dom_naomi_files/Datim_DR_shapefile.zip", 
             id_map = "DOM/2022-11-09-initial_dom_naomi_files/dom_area_id_map_2023.csv"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)

# Read in files from Sharepoint
prov <- read_sf_zip(files$prov)
datim <- read_sf_zip(files$datim)

pryr::object_size(prov)
pryr::object_size(datim)

#' Simplify boundaries to reduce file size (both ~3.5 Mb)
prov_simple <- ms_simplify(prov, keep = 0.25)
pryr::object_size(prov_simple)

datim_simple <- ms_simplify(datim, keep = 0.25)
pryr::object_size(datim_simple)


ggplot() +
  geom_sf(data = prov_simple, colour = "red", fill = NA) +
  geom_sf(data = datim_simple, colour = "black", fill = NA)

# Datim and local boundaries look very simillar
id_map <- read_csv(files$id_map) %>%
  mutate(area_name = if_else(area_level == 1 & area_name != 10, paste0("0", area_name), 
                             area_name)) 
  
dom_wide <- prov_simple %>%
  select(name1 = REG, name2 = TOPONIMIA) %>%
  arrange(name1, name2) %>%
  left_join(id_map, by = c("name1" = "area_name")) %>%
  select(name1, id1 = area_id_2023, name2) %>%
  left_join(id_map, by = c("name2" = "area_name")) %>%
  mutate(id0 = "DOM", name0 = "Dominican Republic", spectrum_region_code = 0L) %>%
  select(name0, id0, name1, id1, name2, id2 = area_id_2023, spectrum_region_code) 

dom_long <- gather_areas(dom_wide)
  
#' Create boundaries file
dom_areas <- dom_long %>%
  mutate(area_sort_order = row_number(),
         center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = area_level %>%
           recode(`0` = "Country",
                  `1` = "Region",
                  `2` = "Province"),
         display = TRUE) %>%
  select(area_id, area_name, parent_area_id, area_level, area_level_label,
         spectrum_region_code, display, area_sort_order,
         center_x, center_y, geometry)

#' Save boundaries
sf::st_write(dom_areas, "dom_areas.geojson", delete_dsn = TRUE)

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(dom_areas)
ggsave("check/dom-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

dev.off()
