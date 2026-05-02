-- Ingest Data from data/processed folder --
-- DISCLAIMER: This code can only be run once to ingest the data. If ingested inccorently, 
-- drop the table with DROP TABLE ____ (table name) query.

--- Species data ---
CREATE TABLE species AS
    SELECT * FROM read_csv_auto('data/processed/species.csv');

--- Protected Areas Data ---
CREATE TABLE protected_areas AS
    SELECT * FROM read_csv_auto('data/processed/protected_areas.csv');

--- Countries Data ---
CREATE TABLE countries AS
    SELECT * FROM read_csv_auto('data/processed/countries.csv');

--- Protection Status Data ---
CREATE TABLE protection_sts AS
    SELECT * FROM read_csv_auto('data/processed/protection_sts.csv');

--- Occurrences RAW Data ---
CREATE TABLE occurrences_raw AS
    SELECT * FROM read_csv_auto('data/processed/occurrences_raw.csv',
  types = {'individual_count': 'VARCHAR'});

--- Ocurrences AGGREGATED Data ---
CREATE TABLE occurrences AS
    SELECT * FROM read_csv_auto('data/processed/occurrences.csv');

