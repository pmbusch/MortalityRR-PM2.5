### MortalityRR-PM2.5
## Aggregate data for CASEN survey
## PBH Nov 2020

# Load Survey Data --------
source("Scripts/Load_Data/casen_load.R", encoding = "UTF-8")

## EDUCATION -----------
# I create variable: % less than high school education
df_education$e6a %>% unique()
df_education <- df_education %>% 
  filter(e6a!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(menor_media=if_else(e6a<8,1,0)) %>% 
  group_by(codigo_comuna,menor_media) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(menor_media==1) %>% 
  select(codigo_comuna,perc) %>% 
  rename(perc_less_highschool=perc)


## HEALTH CARE PROVIDER ---------
# Variable: % Health care provider
# Method is done to include commune with nobody with isapre
df_healthProvider$s12 %>% unique()
df_healthProvider <-  df_healthProvider %>% 
  filter(s12!=99) %>% # filtro respuesta no sabe (unknown)
  mutate(prev=case_when(
    s12==1 ~ "perc_fonasa_A",
    s12==2 ~ "perc_fonasa_B",
    s12==3 ~ "perc_fonasa_C",
    s12==4 ~ "perc_fonasa_D",
    s12==6 ~ "perc_FFAA",
    s12==7 ~ "perc_isapre",
    T~"otro")) %>% 
  group_by(codigo_comuna,prev) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc=hab/sum(hab)*100) %>% 
  ungroup() %>% select(-hab) %>% 
  spread(prev, perc, fill=0) %>% select(-otro)

## MEDICAL TREATMENT ---------
df_health$s28 %>% unique()
# 22: No ha estado en tratamiento por ninguna condicion de salud anteriores
df_health <-  df_health %>% 
  filter(s28!=99) %>% # filtro respuesta no sabe
  group_by(codigo_comuna, s28) %>% 
  summarise(hab=sum(hab,na.rm=T)) %>% 
  mutate(perc_health=hab/sum(hab)*100) %>% 
  ungroup() %>% 
  filter(s28==22) %>% select(-hab, -s28)

## WOOD ---------
df_wood_casen <- df_wood_casen %>% 
  mutate(perc_woodCooking=lena_cocina/hab*100,
         perc_woodHeating=lena_calefaccion/hab*100,
         perc_woodWarmWater=lena_agua/hab*100) %>% 
  select(codigo_comuna, perc_woodCooking, perc_woodHeating, perc_woodWarmWater)

  
## JOIN ALL -------
df_casen <- left_join(df_income, 
                      df_occupancy %>% select(codigo_comuna, perc_occupancy)) %>% 
  left_join(df_education) %>% 
  left_join(df_healthProvider) %>% 
  left_join(df_health) %>% 
  left_join(df_wood_casen) %>% 
  select(-nombre_comuna,-codigo_provincia,-nombre_provincia,
         -codigo_region,-nombre_region)


rm(df_codigoSalud, df_codigoEducacion, df_income, df_healthProvider, df_education,
   df_occupancy, df_wood_casen, df_health)

## EoF