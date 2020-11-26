### MortalityRR-PM2.5
## CASEN: CASEN - National Socioeconomic Characterization Survey
## PBH Nov 2020

library(casen)
library(spatstat) #weighted median

# Download data
# casen::descargar_casen_github(anios=2017, carpeta = "Data/Data_Original/Casen")
df_casen <- read_rds("Data/Data_Original/CASEN/2017.rds")
df_casen %>% names()

## Unify codigo_comuna: add 0 to regions (01)
df_casen <- df_casen %>% 
  mutate(comuna=paste(if_else(str_length(comuna)==4,"0",""),
                      comuna,sep=""))

# Expansion Factor: commune and region
df_casen$expc %>% sum()
df_casen$expr %>% sum()


## INCOME -------
# ytotcor: monthly income per capita (corrected). (Ingreso total corregido)
df_income <- df_casen %>% 
  group_by(comuna) %>% 
  summarise(income_mean=weighted.mean(ytotcor,w = expc,na.rm=T),
            income_median=weighted.median(ytotcor, w = expc,na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

## EDUCATION ------------
# e6a: Cuál fue el nivel educacional más alto alcanzado o el nivel educacional actual
# e6a: , highest education leve
df_codigoEducacion <- read_excel("Data/Data_Original/CASEN/Codigos_CASEN.xlsx", sheet = "e6a")
df_education <- df_casen %>% 
  group_by(comuna,e6a) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoEducacion, by=c("e6a"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)


## HEALTH CARE PROVIDER -------
# s12: A qué sistema previsional de salud pertenece usted
# s12: affiliation to health care provider (private or state level
df_codigoSalud <- read_excel("Data/Data_Original/Casen/Codigos_CASEN.xlsx", sheet = "s12")
df_healthProvider <- df_casen %>% 
  group_by(comuna,s12) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(df_codigoSalud, by=c("s12"="codigo")) %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

## MEDICAL TREATMENT -------
# s28: Durante los últimos 12 meses, ¿ha estado en tratamiento médico por..?
# 22: No ha estado en tratamiento por ninguna condicion de salud anteriores
# s28: medical treatment (last year),

df_health <- df_casen %>% 
  group_by(comuna,s28) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)


## occupancy rate -----------
# o9a: o9a. ¿Cuál es su ocupación u oficio?
# o1: La semana pasada, ¿trabajó al menos una hora, sin considerar los quehaceres del hogar?
# o9a: occupancy rate (last week)
df_occupancy <- df_casen %>% 
  filter(!is.na(o1)) %>% 
  group_by(comuna,o1) %>% 
  summarise(hab=sum(expc,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100,
         o1=if_else(o1==1,"Si","No")) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)
# Leave only percentaje of occupancy
df_occupancy <- df_occupancy %>% filter(o1=="Si") %>% rename(perc_occupancy=perc)


## WOOD -----------
# v36a: Qué combustible o fuente de energía usa habitualmente para: Cocinar 
# v36b: Idem Calefacción
# v36c: Idem Sistema de Agua Caliente
# Opción 4: Leña o derivados (pellets, astillas o briquetas)
# Main fuel used for: cooking (v36a), heating (v36b), warm water (v36c)
# Sum if to wood options
df_wood_casen <- df_casen %>% 
  group_by(comuna) %>% 
  summarise(hab=sum(expc, na.rm=T),
            lena_cocina=sum(if_else(v36a==4,expc,0),na.rm=T),
            lena_calefaccion=sum(if_else(v36b==4,expc,0),na.rm=T),
            lena_agua=sum(if_else(v36c==4,expc,0),na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("comuna"="codigo_comuna")) %>% 
  rename(codigo_comuna=comuna)

## EoF