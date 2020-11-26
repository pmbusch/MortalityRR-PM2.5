### MortalityRR-PM2.5
## Hospital beds at commune level
## PBH Nov 2020

# Load Data--------
source("Scripts/Load_Data/Hospitalbeds_load.R", encoding = "UTF-8")

## Summarise by commune ----------
df_hospitalBeds <- df_camas %>% 
  group_by(codigo_comuna) %>% 
  summarise(hospitalBeds=sum(total,na.rm=T)) %>% ungroup() %>% 
  left_join(codigos_territoriales) %>% 
  select(codigo_comuna,hospitalBeds)

# Limpio WS
rm(df_camas_priv, df_camas_pub, df_establecimiento, df_camas)

## EoF