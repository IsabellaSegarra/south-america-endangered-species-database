-- What country has the highest amount of species occurrences that fall inside protected areas?

SELECT c.country_name, COUNT(*) as total_occurrences FROM occurrences o
    JOIN protection_status ps ON o.species_id = ps.species_id
    JOIN countries c ON o.country_code = c.country_code
    WHERE ps.in_protected_area = TRUE
    GROUP BY o.country_code, c.country_name
    ORDER BY total_occurrences DESC;

-- What is the population trend for the Black-backed Tanager --
