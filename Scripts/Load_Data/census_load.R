### MortalityRR-PM2.5
## Load Census 2017 data
## PBH Nov 2020

## Load data-------------
df_censo <- read_delim("Data/Data_Original/Census/Censo2017_Manzanas.csv",
                       delim = ";", na = c("NA","*"),
                       col_types = "cccccccccddddddddddddddddddddddddddddddddddddddddddddddcccc",
                       locale = locale(encoding = "windows-1252"))
df_censo %>% names()

## Add 0 to commune codes
df_censo <- df_censo %>% 
  mutate(COMUNA=paste(if_else(str_length(REGION)==1,"0",""),
                      COMUNA,sep=""))

## Note: Population per group age is obtained using Chilemapas datasets


## URBAN-RURAL -------------
df_rural <- df_censo %>% 
  group_by(COMUNA, AREA) %>% 
  summarise(poblacion=sum(PERSONAS,na.rm=T)) %>% 
  mutate(perc_rural=poblacion/sum(poblacion)*100) %>%
  filter(AREA=="2") %>% # 2:Rural area
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)


## ethnicity origin ---------
# PUEBLO: Total de personas que se consideran pertenecientes a un pueblo indigena u originario
df_puebloOrig <- df_censo %>% 
  group_by(COMUNA) %>% 
  summarise(poblacion=sum(PERSONAS,na.rm=T),
            pueblo=sum(PUEBLO,na.rm=T),
            perc_ethnicityOrig=pueblo/poblacion*100) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)

## Housing ------------
# Vivienda: corresponden a los lugares de alojamiento, estructuralmente separados e independientes, en los que pueden residir las personas. Estas pueden ser viviendas particulares o viviendas colectivas.

df_viviendas <- df_censo %>% 
  group_by(COMUNA) %>% 
  summarise(houses=sum(TOTAL_VIV,na.rm=T)) %>% 
  ungroup() %>% 
  left_join(codigos_territoriales, by = c("COMUNA"="codigo_comuna")) %>% 
  rename(codigo_comuna=COMUNA)
df_viviendas$houses %>% sum()

## EoF