#' ## Canmeroon (CMR)
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
urls <- list( humdata = "CMR/2020-01-06/cmr_admbnda_inc_20180104_shp.zip",
              pepfar = "CMR/2019-25-06/Cameroon_HealthDistricts12012016.zip",
              dist = "CMR/2020-01-06/CameroonHierarchie.xlsx",
              raw = "CMR/2020-01-18/cmr_areas.geojson.zip", 
              boundaries_2023 = "CMR/2022-11-30%20190%20to%20197%20districts%20(WHO)/Cameroon%20197,%20from%20WHO%202022%2012.zip", 
              districts_2023 = "CMR/2022-11-30%20190%20to%20197%20districts%20(WHO)/2022_11_30_cmr_hierarachy_wide.xlsx"
) %>%
  lapply(function(x) file.path(naomi_raw_path, x)) %>%
  lapply(URLencode)

files <- lapply(urls, sharepoint$download)


humdata <- read_sf_zip_list(files$humdata) %>% bind_rows()
pepfar <- read_sf_zip(files$pepfar)

new <- read_sf_zip(files$dist_2022)

#' Health districts
dist <- readxl::read_excel(files$dist, 1) %>%
  rename(region = 1, health_district = 2)

#' Statistical divisions:
stat <- readxl::read_excel(files$dist, 2)

pepfar %>%
  st_drop_geometry()  %>%
  distinct(level4na_1, level5na_1, uid) %>%
  mutate(health_district = level5na_1 %>%
           recode("Saa" = "Sa'a",
                  "Garoua 1" = "Garoua I",
                  "GAROUA 2" = "Garoua II",
                  "Malentouen" = "Malantouen",
                  "Mbangue" = "Bangue")) %>%
  full_join(dist %>% mutate(hsd = 1),
            by = c("level4na_1" = "region", "health_district")) %>%
  filter(is.na(uid) | is.na(hsd))

p_nregion_fotokol <- ggplot(filter(pepfar, level4na_1 == "Extreme Nord")) +
  geom_sf() +
  geom_sf_label(aes(label = if_else(name_1 == "Makary", "Makary", NA_character_))) +
  geom_sf(aes(fill = ADM3_FR), data = filter(humdata, ADM3_FR == "Fotokol"), alpha = 0.5) +
  naomi::th_map() +
  labs(x = NULL, y = NULL) +
  ggtitle("CMR: Extreme Nord region with Fotokol commune overlaid")


ggsave("check/check-overlay-north-region-fotokol.png", p_nregion_fotokol, h = 7, w = 7)


#' ## Edits to shape file
raw <- read_sf_zip(files$raw, pattern = "geojson$")

#' Recode types and accents
raw <- raw %>%
  mutate(area_level = as.integer(area_level),
         display = as.logical(display),
         area_name = stringr::str_replace(area_name, "\xe9", "é") %>%
           recode("Centre" = "Centre (sans Yaoundé)",
                  "Littoral" = "Littoral (sans Douala)",
                  "Yaounde" = "Yaoundé",
                  "North West" = "Nord-Ouest",
                  "Extreme Nord" = "Extreme-Nord",
                  "South West" = "Sud-Ouest"))


#' Simplify boundaries to reduce file size
raw_wide <- spread_areas(raw)

cmr_wide1 <- raw_wide %>%
  rmapshaper::ms_simplify(0.05)

cmr_wide2 <- raw_wide %>%
  rmapshaper::ms_simplify(0.02)

pryr::object_size(raw_wide)
pryr::object_size(cmr_wide1)
pryr::object_size(cmr_wide2)

raw_wide %>%
  split(1:nrow(.)) %>%
  sapply(pryr::object_size) %>%
  `/`(1e6) %>%
  summary()

cmr_wide1 %>%
  split(1:nrow(.)) %>%
  sapply(pryr::object_size) %>%
  `/`(1e6) %>%
  summary()

cmr_wide2 %>%
  split(1:nrow(.)) %>%
  sapply(pryr::object_size) %>%
  `/`(1e6) %>%
  summary()

ggplot() +
  geom_sf(data = cmr_wide2, color = "red", fill = NA) +
  geom_sf(data = cmr_wide1, color = "black", fill = NA)


cmr <- cmr_wide2 %>%
  rename_all(~sub("area\\_", "", .)) %>%
  mutate(spectrum_region_code = 0L) %>%
  gather_areas()

pryr::object_size(cmr)

cmr_areas_2022 <- cmr %>%
  left_join(select(st_drop_geometry(raw), area_id, area_sort_order)) %>%
  mutate(center = sf::st_point_on_surface(geometry),
         center_x = sf::st_coordinates(center)[,1],
         center_y = sf::st_coordinates(center)[,2],
         center = NULL,
         area_level_label = recode(area_level,
                                   `0` = "Pays",
                                   `1` = "Région",
                                   `2` = "Département",
                                   `3` = "District Sanitaire"),
         display = TRUE) %>%
  select(names(raw)) %>%
  arrange(area_level, area_sort_order)


# Read in new 2023 boundaries
district_names_2023 <- read_xlsx(files$districts_2023) %>%
  mutate(area_name = `District sanitaire\r\n(In DHIS, New)`, 
         area_name = area_name %>% sub("District ", "", .))

# Map geomtery to area names from DHIS
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
                            "Kumba" = "Kumba South",           
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
  left_join(district_names_2023 %>% select(area_id = area_id3, area_name)) %>%
  st_as_sf()


# Compare new boundaries to old boundaries
district_boundaries_2022 <- cmr_areas_2022 %>% filter(area_level == 3) %>% st_as_sf()

ggplot() +
  geom_sf(data = district_boundaries_2022, colour = "black", fill = NA) +
  geom_sf(data = district_boundaries_2023, colour = "red", fill = NA)


                            
                            
                            
                            
                            


district_boundaries_2023 %>% filter(is.na(area_id))


new_districts <- filter(district_names_2023, is.na(area_name3))



overlaps <- st_overlaps(districts_2022, new_districts)







cmr_area_hierarchy <- cmr_areas %>%
  st_set_geometry(NULL) %>%
  select(area_id, area_name, area_level, parent_area_id, area_sort_order, center_x, center_y, spectrum_region_code)


#' Save boundaries
sf::st_write(cmr_areas, "cmr_areas.geojson", delete_dsn = TRUE)
write_csv(cmr_area_hierarchy, "cmr_area_hierarchy.csv")

#' Plot hierarchy
hierarchy_plot <- plot_area_hierarchy_summary(cmr_areas)

ggsave("check/cmr-hierarchy-plot.png", hierarchy_plot, h = 6, w = 12)
dev.off()
