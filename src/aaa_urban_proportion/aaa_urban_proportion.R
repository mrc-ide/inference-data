iso3_c <- iso3
areas <- areas <- read_area_merged("depends/naomi_areas.geojson") %>%
  mutate(iso3 = iso3_c)
unpd <- read_csv("wpp_urban_proportions.csv") %>%
  mutate(iso3 = countrycode::countrycode(country.name, "country.name", "iso3c")) %>%
  filter(iso3 == iso3_c) %>%
  pull(urban_prop)

path <- file.path("https://data.worldpop.org/GIS/Population_Density/Global_2000_2020_1km_UNadj/2020", iso3_c, paste0(tolower(iso3_c), "_pd_2020_1km_UNadj.tif"))
tmpf <- tempfile(fileext = ".tif")
on.exit(unlink(tmpf, force = TRUE))
download.file(path, tmpf)
rast <- raster(tmpf)
density <- exact_extract(rast, areas)

totpop_path <- file.path("https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/", iso3_c, paste0(tolower(iso3_c), "_ppp_2020_1km_Aggregated_UNadj.tif"))
tmpf_totpop <- tempfile(fileext = ".tif")
on.exit(unlink(tmpf_totpop, force = TRUE))
download.file(totpop_path, tmpf_totpop)
rast_totpop <- raster(tmpf_totpop)
population <- exact_extract(rast_totpop, areas)

names(density) <- names(population) <- areas$area_id

population_nat <- population %>% 
  bind_rows(.id = "area_id") %>%
  rename(population = value,
         pop_wt = coverage_fraction) %>%
  filter(area_id == iso3_c)

target <- unpd/100
density_val <- 200
abs_diff <- 1

density_nat <- density %>%
  bind_rows(.id = "area_id") %>%
  filter(area_id == iso3_c) %>%
  bind_cols(population_nat %>% dplyr::select(-area_id)) %>%
  filter(!is.na(value))

while(abs_diff > 0.005) {
  
  urban_proportion_nat <- density_nat %>%
    mutate(is_urban = ifelse(value > density_val, 1, 0),
           weighted_pop = population * pop_wt) %>%
    group_by(area_id) %>%
    summarise(urban_proportion = sum(weighted_pop[is_urban == 1])/sum(weighted_pop))
  
  estimate <- urban_proportion_nat %>%
    filter(area_id == iso3_c) %>%
    pull(urban_proportion)
  
  abs_diff <- abs(estimate - target)
  ratio <- estimate/target
  
  density_val <- density_val * ratio
  
  print(abs_diff)
  
}

urban_proportion_full_hierarchy <- density %>%
  bind_rows(.id = "area_id") %>%
  bind_cols(population %>% 
              bind_rows(.id = "area_id") %>%
              rename(population = value,
                     pop_wt = coverage_fraction) %>%
              dplyr::select(-area_id)) %>%
  filter(!is.na(value)) %>%
  mutate(is_urban = ifelse(value > density_val, 1, 0),
         weighted_pop = population * pop_wt) %>%
  group_by(area_id) %>%
  summarise(urban_proportion = sum(weighted_pop[is_urban == 1])/sum(weighted_pop))

write_csv(urban_proportion_full_hierarchy, "urban_proportion.csv")