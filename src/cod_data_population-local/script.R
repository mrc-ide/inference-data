sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
naomi_raw_path <- "sites/HIVInferenceGroup-WP/Shared%20Documents/Data/naomi-raw/"

url <- file.path(naomi_raw_path, "COD/2021-04-14/cod-district-populaton_2021-04-14.csv")
file <- sharepoint$download(url)

# Update with 2022 area_ids
df <- read_csv(file) %>%
  mutate(area_id1 = recode(area_id,
                           "COD_1_3" = "COD_1_3xd",
                           "COD_1_15" = "COD_1_15sl"),
    area_id2 = recode(area_id2,
                           "COD_2_15" = "COD_2_15td",
                            "COD_2_90" = "COD_2_90hs"))

#' Dependencies
areas <- read_sf("depends/naomi_areas.geojson")
worldpop <- read_csv("depends/population_worldpop_naomi.csv")

filter(worldpop, population == 0) %>%
  group_by(area_id) %>% summarise()

#' Worldpop population is 0 for the following districts:
 # COD_3_413, COD_3_414, COD_3_418, COD_3_420, COD_3_422

#  Redistribute Worldpop totals at the admin2 level
areas <- st_drop_geometry(areas)

worldpop_dist <- worldpop %>%
  filter(calendar_quarter == "CY2020Q2") %>%
  semi_join(
    filter(areas, area_level == 2),
    by = "area_id"
  ) %>%
  group_by(area_id) %>%
  transmute(area_id, sex, age_group, proportion = population / sum(population)) %>%
  ungroup()

pop <- df %>%
  full_join(worldpop_dist, by = c("area_id2" = "area_id")) %>%
  mutate(population = `ZS pop` * proportion)

filter(pop, is.na(population)) %>% as.data.frame()


stopifnot(sum(df[["ZS pop"]]) == sum(pop$population))

#' Aggregate to all levels
pop_agesex <- pop %>%
  mutate(
    source = "Unknown",
    calendar_quarter = "CY2019Q2"
  )

pop_agesex <- pop_agesex %>%
  {bind_rows(
     count(., area_id = area_id0, source, calendar_quarter, sex, age_group,
           wt = population, name = "population"),
     count(., area_id = area_id1, source, calendar_quarter, sex, age_group,
           wt = population, name = "population"),
     count(., area_id = area_id2, source, calendar_quarter, sex, age_group,
           wt = population, name = "population"),
     count(., area_id = area_id3, source, calendar_quarter, sex, age_group,
           wt = population, name = "population")
   )}

pop_agesex <- pop_agesex %>%
  left_join(
    select(areas, area_id, area_name),
    by = "area_id"
  ) %>%
  select(area_id, area_name, everything())

pop_agesex$asfr <- NA_real_

validate_naomi_population(pop_agesex, areas, 0:3)

write_csv(pop_agesex, "cod_population_local.csv")
