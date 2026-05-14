-- Database Queries -- 

-- What country has the highest amount of species occurrences that fall inside protected areas?

SELECT c.country_name, COUNT(*) as total_occurrences FROM occurrences o
    JOIN protection_status ps ON o.species_id = ps.species_id
    JOIN countries c ON o.country_code = c.country_code
    WHERE ps.in_protected_area = TRUE
    GROUP BY o.country_code, c.country_name
    ORDER BY total_occurrences DESC;

-- Which protected areas have the most unique species?-- 
SELECT pa.name_eng, pa.country_code, c.country_name, COUNT(DISTINCT ps.species_id) as unique_species
FROM protected_areas pa
JOIN protection_status ps ON pa.pa_id = ps.pa_id
JOIN countries c ON pa.country_code = c.country_code
WHERE ps.in_protected_area = TRUE
GROUP BY pa.name_eng, pa.country_code, c.country_name
ORDER BY unique_species DESC
LIMIT 20;

-- Which countries have the most protected areas? --- 
SELECT pa.country_code, c.country_name, COUNT(DISTINCT pa_id) as total_protected_areas
FROM protected_areas pa
JOIN countries c ON pa.country_code = c.country_code
GROUP BY pa.country_code, c.country_name
ORDER BY total_protected_areas DESC;
