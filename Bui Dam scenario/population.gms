parameter population(*) Population read by year

$call gdxxrw "Ghana population.xlsx" o=ghana_population par=pop_delta Rng='population_15_65!A2:B97' rdim=1 par=pop_total Rng='total_population!A2:B97' rdim=1
