### MortalityRR-PM2.5
## Generate some features, remove Na, clean data
## PBH Nov 2020

## Load all information ------
# source("Scripts/01-LoadAllData.R", encoding = "UTF-8")
source("Scripts/00-Functions.R", encoding = "UTF-8")

data_model %>% names() %>% sort()
# data_model %>% skim()
## Parameters -----------
# Rate of hospital beds
# MP2.5 and fractions
data_model <- data_model %>% 
  mutate(rate_hospitalBeds=hospitalBeds/population*1e5,
         mp25_10um=mp25/10, # to see RR increase per 10 ug/m3
         mp10_minus25=mp10-mp25)

# Generate deaths as count number
data_model <- data_model %>% 
  mutate(deathsAdj_CDP=mrAdj_CDP*population/1e5,
         deathsAdj_CDP=as.integer(deathsAdj_CDP),
         deathsAdj_AllCauses=mrAdj_AllCauses*population/1e5,
         deathsAdj_AllCauses=as.integer(deathsAdj_AllCauses),
         deathsAdj_CVD=mrAdj_CVD*population/1e5,
         deathsAdj_CVD=as.integer(deathsAdj_CVD),
         deathsAdj_RSP=mrAdj_RSP*population/1e5,
         deathsAdj_RSP=as.integer(deathsAdj_RSP),
         deathsAdj_CAN=mrAdj_CAN*population/1e5,
         deathsAdj_CAN=as.integer(deathsAdj_CAN),
         deathsAdj_LCA=mrAdj_LCA*population/1e5,
         deathsAdj_LCA=as.integer(deathsAdj_LCA),
         deathsAdj_ExtCauses=mrAdj_ExtCauses*population/1e5,
         deathsAdj_ExtCauses=as.integer(deathsAdj_ExtCauses))

# Male and Female
data_model <- data_model %>% 
  mutate(deathsAdj_CDP_male=mrAdj_CDP_male*population/1e5,
         deathsAdj_CDP_male=as.integer(deathsAdj_CDP_male),
         deathsAdj_AllCauses_male=mrAdj_AllCauses_male*population/1e5,
         deathsAdj_AllCauses_male=as.integer(deathsAdj_AllCauses_male),
         deathsAdj_CVD_male=mrAdj_CVD_male*population/1e5,
         deathsAdj_CVD_male=as.integer(deathsAdj_CVD_male),
         deathsAdj_RSP_male=mrAdj_RSP_male*population/1e5,
         deathsAdj_RSP_male=as.integer(deathsAdj_RSP_male),
         deathsAdj_CAN_male=mrAdj_CAN_male*population/1e5,
         deathsAdj_CAN_male=as.integer(deathsAdj_CAN_male),
         deathsAdj_LCA_male=mrAdj_LCA_male*population/1e5,
         deathsAdj_LCA_male=as.integer(deathsAdj_LCA_male),
         deathsAdj_ExtCauses_male=mrAdj_ExtCauses_male*population/1e5,
         deathsAdj_ExtCauses_male=as.integer(deathsAdj_ExtCauses_male))
data_model <- data_model %>% 
  mutate(deathsAdj_CDP_female=mrAdj_CDP_female*population/1e5,
         deathsAdj_CDP_female=as.integer(deathsAdj_CDP_female),
         deathsAdj_AllCauses_female=mrAdj_AllCauses_female*population/1e5,
         deathsAdj_AllCauses_female=as.integer(deathsAdj_AllCauses_female),
         deathsAdj_CVD_female=mrAdj_CVD_female*population/1e5,
         deathsAdj_CVD_female=as.integer(deathsAdj_CVD_female),
         deathsAdj_RSP_female=mrAdj_RSP_female*population/1e5,
         deathsAdj_RSP_female=as.integer(deathsAdj_RSP_female),
         deathsAdj_CAN_female=mrAdj_CAN_female*population/1e5,
         deathsAdj_CAN_female=as.integer(deathsAdj_CAN_female),
         deathsAdj_LCA_female=mrAdj_LCA_female*population/1e5,
         deathsAdj_LCA_female=as.integer(deathsAdj_LCA_female),
         deathsAdj_ExtCauses_female=mrAdj_ExtCauses_female*population/1e5,
         deathsAdj_ExtCauses_female=as.integer(deathsAdj_ExtCauses_female))


## Fill NA ----------
# Hospital beds: Not all communes have beds: default=0
# tasa_mortalidadAll: no hubo muertes en el periodo temporal elegido: defecto=0
# Superficie: missing some communes: islas de Chile: pascua, juan fernandez y antartica
data_model <- data_model %>% 
  replace_na(list(rate_hospitalBeds=0, hospitalBeds=0, pda=0))


## Add RM Factor
data_model <- data_model %>% 
  mutate(rm=if_else(region=="M","Metropolitan region","Rest of Chile"))


## Urban density on quantile ------------
data_model <- data_model %>% mutate(quartile_urbanDensity=qgroup(urbanDensity, 5))
data_model %>% group_by(quartile_urbanDensity) %>% 
  summarise(count=n()) %>% arrange(desc(count))

# Combine Fonasa and Wood
data_model <- data_model %>% 
  mutate(perc_fonasa_AB=perc_fonasa_A+perc_fonasa_B,
         perc_fonasa_CD=perc_fonasa_C+perc_fonasa_D,
         perc_wood_avg=(perc_woodCooking+perc_woodHeating+perc_woodWarmWater)/3)

## HDD to average, not sum
data_model <- data_model %>% 
  mutate(heating_degree_15_anual=heating_degree_15_anual/24,
         heating_degree_15_winter=heating_degree_15_winter/24,
         heating_degree_15_summer=heating_degree_15_summer/24,
         heating_degree_15_spring=heating_degree_15_spring/24,
         heating_degree_15_fall=heating_degree_15_fall/24,
         heating_degree_18_anual=heating_degree_18_anual/24,
         heating_degree_18_winter=heating_degree_18_winter/24,
         heating_degree_18_summer=heating_degree_18_summer/24,
         heating_degree_18_spring=heating_degree_18_spring/24,
         heating_degree_18_fall=heating_degree_18_fall/24)

data_model %>% skim()


## Total data used in the models
communes_valid <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population, mp25) %>% 
  na.omit() %>% pull(nombre_comuna)

commune_mp25 <- data_model %>% filter(!is.na(mp25)) %>% pull(nombre_comuna)

data_model <- data_model %>% 
  mutate(commune_valid=nombre_comuna %in% communes_valid)

# Communes excluded without meteorological data
commune_mp25[!(commune_mp25 %in% communes_valid)]
rm(commune_mp25)


## Save Data -------
cat('sep=; \n',file = "Data/Data_Model/Data_Model.csv")
write.table(data_model %>% dplyr::select(-geometry),"Data/Data_Model/Data_Model.csv",
            sep=';',row.names = F, append = T)

saveRDS(data_model, "Data/Data_Model/Data_model.rsd")
save.image(".RData")

## Save data in std format (std: standard test data format)
data_model_std <- data_model %>% 
  gather(variable, valor, -codigo_comuna,-codigo_provincia,-codigo_region,
         -nombre_comuna,-nombre_provincia,-nombre_region,-region,-geometry) %>% 
  select(-geometry)


cat('sep=; \n',file = "Data/Data_Model/Data_Model_std.csv")
write.table(data_model_std,"Data/Data_Model/Data_Model_std.csv",
            sep=';',row.names = F, append = T)
rm(data_model_std)

## EoF