#' ## Cameroon (CMR)
#' Source: UNAIDS
#' Levels:
#'   * 1: Région Sanitaire (12)
#'   * 2: District Sanitaire  (197)
#' Spectrum: National (level 0)
#' EPP: National (level 0)
#' EPP Urban/Rural:
#' PEPFAR PSNU:

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
  mutate(id0 = "CMR", name0 = "Cameroon", spectrum_region_code = 0L) 

cmr_simple <- district_boundaries_2023 %>%
  rmapshaper::ms_simplify(keep = 0.03)

cmr_long <- gather_areas(cmr_simple)

cmr_areas_2022 <- cmr_long %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Région",
                                   `2` = "District Sanitaire"),
         display = TRUE) 

hierarchy_plot <- plot_area_hierarchy_summary(cmr_areas_2022)
dir.create("check")

ggsave("check/cmr-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
dev.off()


#' Save boundaries
sf::st_write(cmr_areas_2022, "cmr_areas.geojson", delete_dsn = TRUE)


