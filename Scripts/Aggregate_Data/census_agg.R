### MortalityRR-PM2.5
## Aggregate census data to commune level
## PBH Nov 2020

# Load Data --------
source("Scripts/Load_Data/census_load.R", encoding = "UTF-8")

## Join all---------------
df_census <- df_puebloOrig %>% 
  left_join(df_viviendas %>% select(codigo_comuna,houses)) %>% 
  left_join(df_rural %>% select(codigo_comuna, perc_rural)) %>% 
  select(codigo_comuna, perc_ethnicityOrig, houses, perc_rural)
  
## Comunas con NA en porcentaje rural corresponde a O
df_census <- df_census %>% 
  mutate(perc_rural=if_else(is.na(perc_rural),0,perc_rural))

rm(df_puebloOrig,df_viviendas, df_rural, df_censo)

## EoF