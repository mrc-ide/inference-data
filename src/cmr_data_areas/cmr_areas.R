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
#' 2023 estimates update:
# * Admin 0 = National (for National HIV estimates)
# * Admin 1 = 10 régions (for PEPFAR COP planning and regional planning)
# * Admin 2 = 10 régions + 2 villes (for regional planning and city level planning 
#                                    as it also takes into account 2 major cities)
# * Admin 3 = 197 districts for district level planning

## sf v 1.0.0 update changes to use s2 spherical geometry as default
## This creates issues for DHS coordinate data extraction scripts
## Revert back to planar geometry
sf::sf_use_s2(FALSE)
dir.create("check")

#' Authenticate SharePoint login
sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")

#' Read in files from SharePoint
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"


#' Read in files from SharePoint
urls <- list( boundaries_2023 = "CMR/2022-11-30%20190%20to%20197%20districts%20(WHO)/Cameroon%20197,%20from%20WHO%202022%2012.zip", 
              districts_2023 = "CMR/2022-11-30%20190%20to%20197%20districts%20(WHO)/2022_11_30_cmr_hierarchy_wide.xlsx"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)

# Read in new 2023 boundaries
district_names_2023 <- read_xlsx(files$districts_2023, sheet = 1) %>%
  mutate(area_name = `District sanitaire\r\n(In DHIS, New)`, 
         area_name = area_name %>% sub("District ", "", .))

# Map geometry to area names from DHIS
district_boundaries_2023 <- read_sf_zip(files$boundaries_2023) %>%
  select(area_name = District_S) %>%
  mutate(area_name = recode(area_name, 
                            "Bétaré Oya" = "Betare Oya",    
                            "Biyem-Assi" = "Biyem Assi",      
                            "Cité des palmiers" = "Cite Des Palmiers",
                            "Elig-Mfomo" = "Elig Mfomo",       
                            "Eyumojock" = "Eyumodjock",      
                            "Garoua 1" = "Garoua I",        
                            "Garoua 2" = "Garoua II",       
                            "Garoua Boulaï" = "Garoua Boulai", 
                            "Gueré" = "Guere",            
                            "Kumba" = "Kumba-South",           
                            "Kumba North" = "Kumba-North",     
                            "Makari" = "Makary",         
                            "Mvog Ada" = "Mvog-Ada",       
                            "Ndélélé" = "Ndelele",         
                            "New-Bell"  = "New Bell",       
                            "Ngaoundéré Rural" = "Ngaoundere Rural",
                            "Ngaoundéré Urbain" = "Ngaoundere Urbain",
                            "Saa" = "Sa'a",             
                            "Tignère" = "Tignere",        
                            "Tokombéré" = "Tokombere",       
                            "Zoétele"  = "Zoetele")) %>%
  left_join(district_names_2023) %>%
  st_as_sf() %>%
  select(id1 = area_id1, name1 = area_name1,
         id2 = area_id3, name2 = area_name) %>%
  mutate(id0 = "CMR", name0 = "Cameroon", spectrum_region_code = 0L, 
         id2 = str_replace(id2, "CMR_3", "CMR_2")) 

cmr_simple <- district_boundaries_2023 %>%
  rmapshaper::ms_simplify(keep = 0.03)

cmr_wide <- cmr_simple %>%
  select(name2 = name1, id3 = id2, name3 = name2, everything()) %>%
  mutate(id3 = str_replace(id3, "CMR_2", "CMR_3"), 
         name1 = recode(name2, 
                        "Littoral (sans Douala)" = "Littoral", 
                        "Douala" = "Littoral", 
                        "Centre (sans Yaoundé)" = "Centre", 
                        "Yaoundé" = "Centre")) %>%
  arrange(name2) %>%
  group_by(name1) %>%
  mutate(id1 = paste0("CMR_1_", cur_group_id()), 
         id2 = str_replace(id1, "CMR_1", "CMR_2"), 
         id2 = case_when(name2 == "Douala" ~ "CMR_2_11", 
                         name2 == "Yaoundé" ~ "CMR_2_12", 
                         TRUE ~ id2)) %>%
  select(id0, name0, id1, name1, id2, name2,id2, name3, everything()) %>%
  arrange(name1, name2)

cmr_long <- gather_areas(cmr_wide)

cmr_areas_2022 <- cmr_long %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
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




hierarchy_plot <- plot_area_hierarchy_summary(cmr_areas_2022)
dir.create("check")

ggsave("check/cmr-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)

while (!is.null(dev.list())) dev.off()

#' Save boundaries
sf::st_write(cmr_areas_2022, "cmr_areas.geojson", delete_dsn = TRUE)

