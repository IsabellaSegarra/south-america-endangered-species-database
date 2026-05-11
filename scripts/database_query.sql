-- What ascountry has the highest amount of species occurrences that fall inside protected areas?

SELECT c.country_name, COUNT(*) as total_occurrences FROM occurrences o
    JOIN protection_status ps ON o.species_id = ps.species_id
    JOIN countries c ON o.country_code = c.country_code
    WHERE ps.in_protected_area = TRUE
    GROUP BY o.country_code, c.country_name
    ORDER BY total_occurrences DESC;

WITH ranked_species AS (
    SELECT 
        o.country_code,
        c.country_name,
        s.scientific_name,
        cn.common_name_en,
        COUNT(*) as total_occurrences,
        ROW_NUMBER() OVER (PARTITION BY o.country_code ORDER BY COUNT(*) DESC) as rank
    FROM occurrences_raw o
    JOIN protection_status ps ON o.species_id = ps.species_id
    JOIN species s ON o.species_id = s.species_id
    JOIN common_names cn ON o.species_id = cn.species_id
    JOIN countries c ON o.country_code = c.country_code
    WHERE ps.in_protected_area = TRUE
    GROUP BY o.country_code, c.country_name, s.scientific_name, cn.common_name_en
)
SELECT * FROM ranked_species WHERE rank = 1;



-- Which species have the most occurrences but are NOT in any protected area?--
SELECT s.scientific_name, cn.common_name_en, COUNT(*) as total_occurrences
FROM occurrences_raw o
JOIN species s ON o.species_id = s.species_id
JOIN common_names cn ON o.species_id = cn.species_id
JOIN protection_status ps ON o.species_id = ps.species_id
WHERE ps.in_protected_area = FALSE
GROUP BY s.scientific_name, cn.common_name_en
ORDER BY total_occurrences DESC
LIMIT 20;

-- Which protected areas have the most unique species?-- 
SELECT pa.name_eng, pa.country_code, c.country_name, COUNT(DISTINCT ps.species_id) as unique_species
FROM protected_areas pa
JOIN protection_status ps ON pa.pa_id = ps.pa_id
JOIN countries c ON pa.country_code = c.country_code
WHERE ps.in_protected_area = TRUE
GROUP BY pa.name_eng, pa.country_code, c.country_name
ORDER BY unique_species DESC
LIMIT 20;

