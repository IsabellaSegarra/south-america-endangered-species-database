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

pa <- st_read(here("data","raw", "WDPA_WDOECM_Apr2026_Public_SA_shp_0"))

# GBIF vernacular/common names data ----
vernacular <- read_tsv(here("data", "raw", "VernacularName.tsv"))

#......Data cleaning......

# GBIF vernacular names
vernacular_en_sp <- vernacular %>%
  clean_names() %>% 
  filter(language %in% c("en", "es")) %>% 
  pivot_wider(names_from = language, values_from = vernacularName) %>% 
  select(en, es, taxonID)

common_names_tbl <- vernacular %>%
  clean_names() %>% 
  filter(language %in% c("en", "es")) %>%
  group_by(taxon_id, language) %>%
  slice(1) %>%
  ungroup() %>%
  pivot_wider(id_cols = taxon_id, names_from = language, values_from = vernacular_name)

common_names_tbl <- left_join(species_tbl, common_names_tbl, by = c("species_key = taxon_id"))


# gbif ---- Produce tables 

# Species table
species_tbl <- gbif %>%
  clean_names() %>%
  select(species_key, scientific_name, genus, family, order, class, phylum, kingdom, taxon_rank) %>%
  distinct(species_key, .keep_all = TRUE) %>% 
# Remove author citation regex 
 mutate(scientific_name = sub(" [\\(A-Z].*$", "", scientific_name))

# Clean up scientific name 
# join common names back 

# Occurrence table
occurrences_tbl <- gbif %>%
  clean_names() %>% 
  select(species_key, country_code, decimal_latitude, decimal_longitude, event_date, year, month, day, occurrence_status, individual_count, coordinate_uncertainty_in_meters, institution_code) %>% 
  filter(
    !is.na(decimal_latitude),
    !is.na(decimal_longitude),
    coordinate_uncertainty_in_meters < 10000)  %>% # drop anything with >10km uncertainty
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = 4326) %>% 
  mutate(event_date = as_datetime(event_date))
  

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

# Aggregated occurrences table
species_occurrence_tbl <- occurrences_tbl %>%
  group_by(species_key, country_code, year) %>%
  summarise(
    n_occurrences = n(),
    .groups = "drop"
  )

#..... Protected Areas ......

# join with occurence, aggregate points - create binary indicator (within PA YES/NO --> protected_area ID)

protected_areas_tbl <- pa %>% 
  clean_names() %>% 
  mutate(pa_id = site_id, 
         name_sp = name, 
         desig_sp = desig
         ) %>% 
  select(pa_id, name_eng, name_sp, desig_eng, desig_sp, desig_type,iucn_cat, realm, status_yr, gov_type, geometry)


# Species protection status table 

# -- Diagnositc checks --
st_crs(occurrences_tbl) == st_crs(pa)

protected_areas_tbl <- st_make_valid(protected_areas_tbl)
occurrences_tbl <- st_make_valid(occurrences_tbl)

# check if points overlap
plot(st_geometry(pa))
plot(st_geometry(occurrences_tbl), add = TRUE, col = "red")

st_bbox(occurrences_tbl)
st_bbox(protected_areas_tbl)

# --- Now join---

# Transform to a projected CRS for accurate buffering in metres
occurrences_proj <- st_transform(occurrences_tbl, 3857)
pa_proj <- st_transform(protected_areas_tbl, 3857)

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

