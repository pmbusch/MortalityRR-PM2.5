# MortalityRR-PM2.5
Chronic Exposure to Fine Particles (PM2.5) and Mortality: A case study from Chile

An ecological cross-sectional study with national data at commune level from Chile to address the impact of long term PM2.5 concentrations on age-adjusted mortality rates, controlling by multiple explanatory variables

Project has the following sections:
* Compiled data for analysys [`Data`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Data)
* Code for analysis [`Scripts`](https://github.com/pmbusch/MortalityRR-PM2.5/tree/master/Scripts)

# Data Sources
* **Mortality**: DEIS-Health Statistics and Information Department (https://deis.minsal.cl/#datosabiertos)
	* Death certificates at individual level for the period 2017-2019. Detail of age, sex, commune and specific cause of death.
* **Air pollution**: SINCA-National Air Quality Information System (https://sinca.mma.gob.cl/)
	* Hourly monitor data of PM2.5 concentrations for the period 2017-2019. Multiple monitor stations with data available across Chile.
* **Meteorology**: SINCA-National Air Quality Information System (https://sinca.mma.gob.cl/)
	* Hourly monitor data of air temperature and relative humidity for the period 2017-2019. Multiple monitor stations with data available across Chile.
* **Demography**: CENSO-National Census Bureau 2017 (https://www.censo2017.cl/)
	* Population data at census district for 2017. Detail of age, sex, urban-rural and ethnicity origin (binary).
	* Index of overcrowding in Housing (habitants per household) per commune. MINVU - Ministry of housing and urbanism (https://www.observatoriourbano.cl/estadisticas-habitacionales/))
* **Socioeconomic**: CASEN 2017-National Socioeconomic Characterization Survey (http://observatorio.ministeriodesarrollosocial.gob.cl/casen-multidimensional/casen/casen_2017.php)
	* Survey data from 2017, with commune expansion factors, of the following variables: monthly income per capita, highest education level, occupancy rate (last week), affiliation to health care provider (private or state level), main fuel used for cooking, heating and warm water.
* **Maps of Chile**: R library *chilemapas* (https://cran.r-project.org/web/packages/chilemapas/index.html)
