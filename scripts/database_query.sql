-- Database Queries -- 

---- What country has the highest amount of species occurrences that fall inside protected areas?----

-- Selet country name and count the number of occurrences 
-- Join with protection status to see which occurrences are within protected areas (TRUE)
-- Group by country and order occurrences by descnending order
SELECT c.country_name, COUNT(*) as total_occurrences FROM occurrences o
    JOIN protection_status ps ON o.species_id = ps.species_id
    JOIN countries c ON o.country_code = c.country_code
    WHERE ps.in_protected_area = TRUE
    GROUP BY o.country_code, c.country_name
    ORDER BY total_occurrences DESC;

---- Which protected areas have the most unique species? ----

-- Select protected area name and count the number of distinct species IDs within
-- Join to protection status and countries where protected area is TRUE
-- Oreder by descending amount of unique species
-- Limit to 10 to see the top 10
SELECT pa.name_eng, c.country_name, COUNT(DISTINCT ps.species_id) as unique_species
FROM protected_areas pa
JOIN protection_status ps ON pa.pa_id = ps.pa_id
JOIN countries c ON pa.country_code = c.country_code
WHERE ps.in_protected_area = TRUE
GROUP BY pa.name_eng, c.country_name
ORDER BY unique_species DESC
LIMIT 10;

---- Which countries have the most protected areas? ---- 

-- Select the country, count the number of distinct protected area IDs
-- Join to country
-- Group by country name and code
-- Order by descending
SELECT pa.country_code, c.country_name, COUNT(DISTINCT pa_id) as total_protected_areas
FROM protected_areas pa
JOIN countries c ON pa.country_code = c.country_code
GROUP BY pa.country_code, c.country_name
ORDER BY total_protected_areas DESC;
