### MortalityRR-PM2.5
## INE: Data for mobile sources. Related to internal combustion sources and traffic accidents
## Source: https://www.ine.cl/estadisticas/economia/transporte-y-comunicaciones/permiso-de-circulacion
## PBH Dec 2020

source("Scripts/00-Functions.R", encoding = "UTF-8")
#Package
library(RODBC)

#Defining the path
datab <- file.path("Data/Data_Original/INE/base-de-datos-permisos-de-circulación.accdb")
channel <- odbcConnectAccess2007(datab)

#reading the individual files inside the Main
df_ine <- sqlFetch(channel,"TBL_VehículosConMotor")
rm(datab, channel)

# Columns Bencinero, Diesel, Gas, Electrico, Otro contain all 
# types of motor vehicles in the commune

# Fix format of data
names(df_ine) <- names(df_ine) %>% f_remover_acentos() %>% 
  str_to_lower() %>% str_replace_all(" ","_")

# Commune
df_ine <- df_ine %>% 
  mutate(glosa_comuna=glosa_comuna %>% f_remover_acentos() %>% 
           str_replace("Ollagüe","Ollague") %>% 
           str_replace("Alto BiobIo","Alto Biobio") %>% 
           str_replace("O'Higgins","OHiggins") %>% 
           str_replace("San Juan de La Costa","San Juan de la Costa")) %>% 
  rename(nombre_comuna=glosa_comuna)

df_ine <- left_join(df_ine, codigos_territoriales,by=c("nombre_comuna"))
df_ine %>% filter(is.na(nombre_region)) %>% pull(nombre_comuna) %>% unique()

# Summarise by commune
df_ine <- df_ine %>% 
  mutate(n_auto=bencinero+diesel+gas+electrico+otro) %>% 
  group_by(codigo_comuna) %>% 
  summarise(n_auto=sum(n_auto, na.rm = T)) %>% ungroup()

# Total vehiculos con motor 5,599,733
df_ine$n_auto %>% sum()
