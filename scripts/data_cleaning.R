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

# ---- Import data ----

# GBIF occurrence data ----
gbif <- fread(here("data", "data_raw", "gbif_sa.csv")) 

# ---- Redlist data 
# Store API token 
api_redlist <- init_api("PVRP8ZPtAyamwnXuuEXLNMv7iiHA7eKKRMwp")  

# There is no way to query the api database, use gbif species instead
# Get unique species from your GBIF data
species_list <- unique(gbif[, .(genus, species)])


# Protected areas ----

pa <- st_read(here("data","data_raw", "WDPA_WDOECM_Apr2026_Public_SA_shp_0"))


#---- Data cleaning ----

# gbif ---- Produce tables 

# Species table
species_tbl <- gbif %>%
  clean_names() %>%
  select(species_key, scientific_name, genus, family, order, class, phylum, kingdom, taxon_rank) %>%
  distinct(species_key, .keep_all = TRUE)

# Occurrence table
occurrences_tbl <- gbif %>%
  clean_names() %>% 
  select(species_key, country_code, decimal_latitude, decimal_longitude, event_date, year, month, day, occurrence_status, individual_count, coordinate_uncertainty_in_meters, institution_code) %>% 
  filter(
    !is.na(decimal_latitude),
    !is.na(decimal_longitude),
    coordinate_uncertainty_in_meters < 10000)  %>% # drop anything with >10km uncertainty
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = 4326)
  

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

# Export as csv into data_processed folder 


# 
