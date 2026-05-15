##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- IMPORT DATA --------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# GBIF occurrence data ----
gbif <- fread(here("data", "raw", "gbif_sa.csv")) 

# Protected areas ----
# Load all 3 protected area (polygons (pa_0), polygons (pa_1), and points(p_2))

#Polygons
pa_0 <- st_read(here("data","raw", "WDPA_WDOECM_Apr2026_Public_SA_shp_0", "WDPA_WDOECM_Apr2026_Public_SA_shp-polygons.shp"))

# Polygons
pa_1 <- st_read(here("data","raw","WDPA_WDOECM_Apr2026_Public_SA_shp_1", "WDPA_WDOECM_Apr2026_Public_SA_shp-polygons.shp"))

# Point file 
pa_2 <- st_read(here("data","raw", "WDPA_WDOECM_Apr2026_Public_SA_shp_2", 
                     "WDPA_WDOECM_Apr2026_Public_SA_shp-polygons.shp"))

# Bind all data together 
pa_raw <- bind_rows(pa_0, pa_1, pa_2)

# GBIF vernacular/common names data ----
vernacular <- read_tsv(here("data", "raw", "VernacularName.tsv"))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- DATA CLEANING  -------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~
##  ~ GBIF  ----
##~~~~~~~~~~~~~~

# ---- Species table----
species_tbl <- gbif %>%
  # Standardize column names to snake_case
  clean_names() %>%
  select(species_key, scientific_name, genus, family, order, class, phylum, kingdom, taxon_rank) %>%
  distinct(species_key, .keep_all = TRUE) %>% 
  # Remove author citation regex 
  mutate(scientific_name = sub(" [\\(A-Z].*$", "", scientific_name)) %>% 
  # Rename species key to species id 
  rename(species_id = species_key)

# ----- Raw Occurrence table with geometry ----- 
raw_occurrences_tbl <- gbif %>%
  # Standardize column names to snake_case
  clean_names()%>% 
  filter(
    !is.na(decimal_latitude), # Filter any rows with NA latitude
    !is.na(decimal_longitude), # Filter any rows with NA longitude 
    coordinate_uncertainty_in_meters < 10000, # Drop anything with >10km uncertainty
    occurrence_status == "PRESENT")  %>% # Drop any occurrences that are absent
  st_as_sf(coords = c("decimal_longitude", "decimal_latitude"), crs = 4326) %>% # Georeference
  mutate(event_date = as_date(event_date), 
         individual_count = replace_na(individual_count, 0), 
         # Impute event date from month, day, and year columns
         event_date = case_when(
           !is.na(event_date) ~ event_date,
           !is.na(year) & !is.na(month) & !is.na(day) ~ as_date(paste(year, month, day, sep = "-")),
           !is.na(year) & !is.na(month) ~ as_date(paste(year, month, "01", sep = "-")),
           !is.na(year) ~ as_date(paste(year, "01", "01", sep = "-")),
           TRUE ~ NA_Date_)) %>% 
  select(species_key, country_code, event_date, year,  individual_count) %>% 
  # Rename species key to species id 
  rename(species_id = species_key) %>% 
  # Drop remaining data where there is an NA for event date and year 
  filter(!is.na(event_date), !is.na(year))

# ----- Raw Occurrence table w/o geometry ----- 
raw_occurrences_no_geom <- raw_occurrences_tbl %>% st_drop_geometry()

# ----- Aggregated occurrences -----
# Aggregated occurrences per year 
occurrences_tbl <- raw_occurrences_tbl %>%
  st_drop_geometry() %>%
  group_by(species_id, country_code, year) %>%
  summarize(occurrence = sum(individual_count, na.rm = TRUE), .groups = "drop")


# -----Country table -----
countries_tbl <- gbif %>%
  # Standardize column names to snake_case
  clean_names() %>%
  distinct(country_code) %>%
  filter(country_code %in% c("CO", "BR", "VE", "AR", "CL", "PE", "BO", "EC", "UY")) %>%
  # Create country name column from country codes 
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

##~~~~~~~~~~~~~~~~~~
## ~ VERNACULAR  ----
##~~~~~~~~~~~~~~~~~~

# -----Common names table----- 
common_names_tbl <- vernacular %>%
  # Standardize column names to snake_case
  clean_names() %>%
  # Keep only English and Spanish names for species in our dataset
  filter(language %in% c("en", "es"),
         taxon_id %in% species_tbl$species_id) %>%
  group_by(taxon_id, language) %>%
  # Take first name per species per language to avoid duplicates
  slice(1) %>%
  ungroup() %>%
  # Reshape from long to wide format 
  pivot_wider(id_cols = taxon_id,
              names_from = language,
              values_from = vernacular_name) %>%
  rename(species_id = taxon_id, common_name_en = en, common_name_es = es)

##~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ PROTECTED AREAS ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~

# join with occurence, create binary indicator (within PA YES/NO --> protected_area ID)

#----- Protected areas w/ geom -----
protected_areas_tbl <- pa_raw %>% 
  # Standardize column names to snake_case
  clean_names() %>% 
  mutate(pa_id = site_id, 
         name_sp = name, 
         desig_es = desig, 
         country_code = iso3,
  ) %>% 
  # Filter protected areas to relevant countries 
  filter(country_code %in% c("BRA", "COL", "ARG", "CHL", "ECU", "PER", "VEN", "URY", "BOL")) %>% 
  select(pa_id, country_code, name_eng, name_sp, desig_eng, desig_es, desig_type, realm, status_yr, gov_type, geometry) %>%
  # Rename country codes to make countries_tbl naming 
  mutate(country_code= case_when(
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

# ----- Protected areas w/o geom -----
protected_areas_no_geom  <-  protected_areas_tbl %>% st_drop_geometry()

#  ----- Protection status table  ----- 

# Transform to a projected CRS 
occurrences_proj <- st_transform(raw_occurrences_tbl, 3857)
pa_proj <- st_transform(protected_areas_tbl, 3857) %>% st_make_valid()

# Transform occurrences to point to match pa 
occurrences_proj <- occurrences_proj %>%
  st_cast("POINT")

# Intersect 
protection_sts_tbl <- st_join(
  occurrences_proj %>% select(species_id, geometry),
  pa_proj %>% select(pa_id, geometry),
  join = st_intersects
) %>%
  # Flag TRUE if occurrence falls inside a protected area, FALSE if no match
  mutate(in_protected_area = !is.na(pa_id))

# Produce protection sts table 
protection_sts_tbl <- protection_sts_tbl %>% 
  st_drop_geometry() %>%
  mutate(pa_id = as.integer(pa_id))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##------------------------------- EXPORT --------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


write_csv(species_tbl, "data/processed/species.csv")
write_csv(raw_occurrences_no_geom, "data/processed/occurrences_raw.csv")
write_csv(occurrences_tbl, "data/processed/occurrences.csv")
write_csv(countries_tbl, "data/processed/countries.csv")
write_csv(protected_areas_no_geom, "data/processed/protected_areas.csv")
write_csv(protection_sts_tbl, "data/processed/protection_sts.csv",  na = "")
write_csv(common_names_tbl, "data/processed/common_names.csv")

