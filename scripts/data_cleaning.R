# Load in packages
library(tidyverse)
library(here)
library(dplyr)
library(sf)
library(stars)
library(data.table)
library(rredlist)
library(janitor)
library(iucnredlist)

# ......Import data ......

# GBIF occurrence data ----
gbif <- fread(here("data", "raw", "gbif_sa.csv")) 

# Redlist data ----

redlist <- read_csv(here("data", "raw", "points_data.csv"))

# Protected areas ----

pa_0 <- st_read(here("data","raw", "WDPA_WDOECM_Apr2026_Public_SA_shp_0", "WDPA_WDOECM_Apr2026_Public_SA_shp-polygons.shp"))


pa_1 <- st_read(here("data","raw","WDPA_WDOECM_Apr2026_Public_SA_shp_1", "WDPA_WDOECM_Apr2026_Public_SA_shp-polygons.shp"))

pa_2 <- st_read(here("data","raw", "WDPA_WDOECM_Apr2026_Public_SA_shp_2", 
                    "WDPA_WDOECM_Apr2026_Public_SA_shp-polygons.shp"))

pa_raw <- bind_rows(pa_0, pa_1, pa_2)


# GBIF vernacular/common names data ----
vernacular <- read_tsv(here("data", "raw", "VernacularName.tsv"))

#......Data cleaning......

# gbif ---- Produce tables 

# Species table
species_tbl <- gbif %>%
  clean_names() %>%
  select(species_key, scientific_name, genus, family, order, class, phylum, kingdom, taxon_rank) %>%
  distinct(species_key, .keep_all = TRUE) %>% 
# Remove author citation regex 
 mutate(scientific_name = sub(" [\\(A-Z].*$", "", scientific_name)) %>% 
  rename(species_id = species_key)

# GBIF vernacular names

# Common names table 
common_names_tbl <- vernacular %>%
  clean_names() %>%
  filter(language %in% c("en", "es"),
         taxon_id %in% species_tbl$species_id) %>%
  group_by(taxon_id, language) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(id_cols = taxon_id,
              names_from = language,
              values_from = vernacular_name) %>%
  rename(species_id = taxon_id, common_name_en = en, common_name_es = es)

# Occurrence table
occurrences_tbl <- gbif %>%
  clean_names()%>% 
  filter(
    !is.na(decimal_latitude),
    !is.na(decimal_longitude),
    coordinate_uncertainty_in_meters < 10000)  %>% # drop anything with >10km uncertainty
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = 4326) %>% 
  mutate(event_date = as_date(event_date), 
         species_id = species_key) %>% 
  select(species_key, country_code, event_date, year, month, day, occurrence_status, individual_count) 
  
  
# Country table 
countries_tbl <- gbif %>%
  clean_names() %>%
  distinct(country_code) %>%
  filter(country_code %in% c("CO", "BR", "VE", "AR", "CL", "PE", "BO", "EC", "UY")) %>%
  mutate(country_name = case_when(
    country_code == "CO" ~ "Colombia",
    country_code == "BR" ~ "Brazil",
    country_code == "VE" ~ "Venezuela",
    country_code == "AR" ~ "Argentina",
    country_code == "CL" ~ "Chile",
    country_code == "PE" ~ "Peru",
    country_code == "BO" ~ "Bolivia",
    country_code == "EC" ~ "Ecuador",
    country_code == "UY" ~ "Uruguay"
  ))

#..... Protected Areas ......

# join with occurence, create binary indicator (within PA YES/NO --> protected_area ID)

protected_areas_tbl <- pa_raw %>% 
  clean_names() %>% 
  mutate(pa_id = site_id, 
         name_sp = name, 
         desig_es = desig, 
         country_code = iso3,
         ) %>% 
  filter(country_code %in% c("BRA", "COL", "ARG", "CHL", "ECU", "PER", "VEN", "URY", "BOL")) %>% 
  select(pa_id, country_code, name_eng, name_sp, desig_eng, desig_es, desig_type, realm, status_yr, gov_type, geometry) %>% mutate(country_code= case_when(
    country_code == "COL" ~ "CO",
    country_code == "BRA" ~ "BR",
    country_code == "VEN" ~ "VE",
    country_code == "ARG" ~ "AR",
    country_code == "CHL" ~ "CL",
    country_code == "PER" ~ "PE",
    country_code == "BOL" ~ "BO",
    country_code == "ECU" ~ "EC",
    country_code == "URY" ~ "UY"
  ))


# Protected status table

# Transform to a projected CRS for accurate buffering in metres
occurrences_proj <- st_transform(occurrences_tbl, 3857)
pa_proj <- st_transform(protected_areas_tbl, 3857)

pa_proj <- st_make_valid(pa_proj)

occurrences_proj <- occurrences_proj %>%
  st_cast("POINT")

# Buffer protected areas by e.g. 10km
pa_buffered <- st_buffer(pa_proj, dist = 10000)

# Now intersect
occurrences_proj <- occurrences_proj %>%
  mutate(in_protected_area = lengths(st_intersects(occurrences_proj, pa_buffered)) > 0)

protection_sts_tbl <- st_join(occurrences_proj %>% select(species_key, geometry),
                            pa_buffered %>% select(pa_id, geometry),
                            join = st_intersects) %>%
  mutate(in_protected_area = !is.na(pa_id))


# Export as csv into data_processed folder 



species_tbl <- write_csv(species_tbl, "data/processed/species.csv")
occurrences_tbl <- write_csv(occurrences_tbl, "data/processed/occurrences.csv")
countries_tbl <- write_csv(countries_tbl, "data/processed/countries.csv")
protected_areas_tbl <- write_csv(protected_areas_tbl, "data/processed/protected_areas.csv")

