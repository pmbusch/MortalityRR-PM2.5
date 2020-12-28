### MortalityRR-PM2.5
## Load all relevant Data Frames
## One script to join them all
## PBH Nov 2020
options(dplyr.summarise.inform=FALSE)

source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")


# Load all data sources used -----
# Population data
source("Scripts/Aggregate_Data/population_agg.R", encoding = "UTF-8")
# Mortality rates data
source("Scripts/Aggregate_Data/deis_agg.R", encoding = "UTF-8")
## Air pollution Data
df_conc <- read_rds("Data/Data_Model/AirPollution_Data.rsd")
df_conc_50km <- read_rds("Data/Data_Model/AirPollution_Data_50km.rsd")
df_conc_100km <- read_rds("Data/Data_Model/AirPollution_Data_100km.rsd")
# Meteorological Data
df_meteo <- read_rds("Data/Data_Model/Meteorology_Data.rsd")
# Census data
source("Scripts/Aggregate_Data/census_agg.R", encoding = "UTF-8")
# MINVU - Ministry of housing and urbanism
source("Scripts/Aggregate_Data/minvu_agg.R", encoding = "UTF-8")
# Socioeconomic survey data
source("Scripts/Aggregate_Data/casen_agg.R", encoding = "UTF-8")
# Hospital beds data
source("Scripts/Aggregate_Data/Hospitalbeds_agg.R", encoding = "UTF-8")


## Join all data ----------------
# Join is based on commune level. KEY: codigo_comuna
data_model <- df_population %>% 
  left_join(df_conc, by=c("codigo_comuna")) %>% 
  left_join(df_hospitalBeds, by=c("codigo_comuna")) %>%
  left_join(df_meteo, by=c("codigo_comuna")) %>% 
  left_join(df_casen, by=c("codigo_comuna")) %>%
  left_join(df_census, by=c("codigo_comuna")) %>%
  left_join(df_minvu, by=c("codigo_comuna")) %>%
  left_join(mrAdj, by=c("codigo_comuna")) %>% 
  left_join(mr, by=c("codigo_comuna"))


# rm(df_pobopulation, df_conc, df_hospitalBeds,df_meteo, df_casen, df_censo,df_minvu)

## Save dataframes
saveRDS(df_population,"Data/Data_Model/RDS_DataSets/df_population.rds")
saveRDS(df_hospitalBeds,"Data/Data_Model/RDS_DataSets/df_hospitalBeds.rds")
saveRDS(df_casen,"Data/Data_Model/RDS_DataSets/df_casen.rds")
saveRDS(df_census,"Data/Data_Model/RDS_DataSets/df_census.rds")
saveRDS(df_minvu,"Data/Data_Model/RDS_DataSets/df_minvu.rds")
saveRDS(mrAdj,"Data/Data_Model/RDS_DataSets/mrAdj.rds")
saveRDS(mr,"Data/Data_Model/RDS_DataSets/mr.rds")


## EoF