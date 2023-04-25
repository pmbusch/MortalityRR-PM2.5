### MortalityRR-PM2.5
## Generate data for map figure for paper. Map was made in QGIS
## PBH Feb 2023


## Load -------
load(".RData")

names(data_model)

# filter to 105 communes
df <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,mp25,
                deathsAdj_AllCauses, mrAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  na.omit()


# filter to 105 communes
df <- df %>% 
  dplyr::select(codigo_comuna,nombre_comuna,mp25,mrAdj_CDP,perc_woodHeating)

skim(df)

# map
shapeFile <- chilemapas::mapa_comunas %>% left_join(df)


st_write(shapeFile, "Data/spatialDataMap.shp", driver="ESRI Shapefile")


# EoF