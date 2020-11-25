### MortalityRR-PM2.5
## Load all relevant Data Frames
## PBH Nov 2020
options(dplyr.summarise.inform=FALSE)

source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")

## Scripts para cargar datos
#carga datos incluida en los scripts de agg


# source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
# source("Scripts/Aggregate_Data/censo_agg.R", encoding = "UTF-8") 
# source("Scripts/Aggregate_Data/minvu_agg.R", encoding = "UTF-8") 
# source("Scripts/Aggregate_Data/casen_agg.R", encoding = "UTF-8") 
# source("Scripts/Aggregate_Data/lena_agg.R", encoding = "UTF-8") 
# 
# source("Scripts/Aggregate_Data/camas_agg.R", encoding = "UTF-8") 

# Mortality rates data
source("Scripts/Aggregate_Data/deis_agg.R", encoding = "UTF-8")

## Air pollution DAta
df_conc <- read_rds("Data/Data_Model/AirPollution_Data.rsd")

# Meteorological Data
df_meteo <- read_rds("Data/Data_Model/Meteorology_Data.rsd")



# Gather population USAR DESPUES POBLACION_AGG
df_population <- df_population %>% 
  group_by(codigo_comuna) %>% 
  summarise(population=sum(poblacion, na.rm=T)) %>% ungroup()
df_population$population %>% sum()


## Join all data ----------------
# Join is based on commune level. KEY: codigo_comuna
data_model <- map_commune %>% 
  left_join(df_population, by=c("codigo_comuna")) %>% 
  left_join(df_conc, by=c("codigo_comuna")) %>% 
  # left_join(df_camas, by=c("codigo_comuna")) %>% 
  left_join(df_meteo, by=c("codigo_comuna")) %>% 
  # left_join(df_casen, by=c("codigo_comuna")) %>% 
  # left_join(df_censo, by=c("codigo_comuna")) %>% 
  # left_join(df_minvu, by=c("codigo_comuna")) %>% 
  # left_join(df_lena, by=c("codigo_comuna")) %>% 
  left_join(mrAdj, by=c("codigo_comuna")) %>% 
  left_join(mr, by=c("codigo_comuna"))


rm(df_poblacion, df_muertes, df_conc, df_camas, df_casos, df_cuarentena,
   df_meteo, df_casen, df_censo,df_minvu, df_pcr, df_pcr_tiempo, df_lena, df_tasaMortalidad,
   df_movilidad, df_grupoEdad, cfr_comunas, df_lena_urbana, df_def, df_incidencia)

## EoF