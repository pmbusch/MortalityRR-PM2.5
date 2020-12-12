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
         deathsAdj_CAN=as.integer(deathsAdj_CAN))

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
         deathsAdj_CAN_male=as.integer(deathsAdj_CAN_male))
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
         deathsAdj_CAN_female=as.integer(deathsAdj_CAN_female))


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

data_model %>% skim()

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