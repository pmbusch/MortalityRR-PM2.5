### MortalityRR-PM2.5
## Aggregate Death data to commune level, with details of sex, age and cause of death
## PBH Nov 2020

# Load Data ---------
# source("Scripts/Load_Data/deis_load.R", encoding = "UTF-8")
# source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")

## Explore data -------
df_deis %>% names()
# df_deis <- df_deis %>% filter(year %in% c(2017:2019))
df_deis$year %>% table()

df_deis %>% group_by(cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(grupo_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_grupo_diag1) %>% summarise(count=n()) %>% arrange(desc(count))

df_deis %>% group_by(cap_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_cap_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(grupo_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_grupo_diag2) %>% summarise(count=n()) %>% arrange(desc(count))


# Age
df_deis$edad_tipo %>% unique()
df_deis %>% group_by(edad) %>% summarise(count=n()) %>% arrange(desc(count))

## Separar en edades mayor a 
edades_mayor <- c(0,30,65,75)


### TO DO:
# Crear funcion para obtener la MR deseada, a patir de los datos brutos de poblacion y
# muertes en el periodo 2017-2019
# Argumentos (causa, filtro edad, filtro sexo)
# crear funcion similara pero para MR estandarizada. Usar datos de poblacion directo de chilemapas




# Loop
df_def_edad <- data.frame()
for (e in edades_mayor){
  
  df_deis_total_filter <- df_deis %>% filter(edad>=e)
  
  ## Grupos de interes
  ## All: Todas las filas
  ## All causes: Causas de morbilidad y mortalidad. is.na(cap_diag2)
  ## Cardio: Enfermedades del sistema circulatorio. cap_diag1==I00-I99
  ## Pulmonar: Enfermedades del sistema respiratorio. cap_diag1==J00-J99
  ## Cardiopulmonar: Suma cardio+pulmonar
  ## Cancer: Tumores [Neoplasias]. cap_diag1==C00-D48
  ## External causes: Suplemento de all causes
  
  ## Agg data ----------
  ## Muertes totales
  df_all <- df_deis_total_filter %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_total=n()) %>% ungroup() %>% 
    replace_na(list(def_total=0))
  df_all$def_total %>% sum()
  
  ## All Causes
  df_allCauses <- df_deis_total_filter %>% 
    filter(is.na(cap_diag2)) %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_allCauses=n()) %>% ungroup() %>% 
    replace_na(list(def_allCauses=0))
  df_allCauses$def_allCauses %>% sum()
  
  ## Cardio
  df_cardio <- df_deis_total_filter %>% 
    filter(cap_diag1=="I00-I99") %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_cardio=n()) %>% ungroup() %>% 
    replace_na(list(def_cardio=0))
  df_cardio$def_cardio %>% sum()
  
  ## Pulmonar
  df_pulmonar <- df_deis_total_filter %>% 
    filter(cap_diag1=="J00-J99") %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_pulmonar=n()) %>% ungroup() %>% 
    replace_na(list(def_pulmonar=0))
  df_pulmonar$def_pulmonar %>% sum()
  
  ## CardioPulmonar
  df_cardioPulmonar <- df_deis_total_filter %>% 
    filter(cap_diag1 %in% c("J00-J99","I00-I99")) %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_cardioPulmonar=n()) %>% ungroup() %>% 
    replace_na(list(def_cardioPulmonar=0))
  df_cardioPulmonar$def_cardioPulmonar %>% sum()
  
  ## Cancer
  df_cancer <- df_deis_total_filter %>% 
    filter(cap_diag1 %in% c("C00-D48")) %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_cancer=n()) %>% ungroup() %>% 
    replace_na(list(def_cancer=0))
  df_cancer$def_cancer %>% sum()
  
  ## Externas
  df_extCauses <- df_deis_total_filter %>% 
    filter(!is.na(cap_diag2)) %>% 
    group_by(codigo_comuna, year) %>% 
    summarise(def_extCauses=n()) %>% ungroup() %>% 
    replace_na(list(def_extCauses=0))
  df_extCauses$def_extCauses %>% sum()
  
  ## Join All -------
  # Base codigo comuna y año
  df_def <- expand.grid(codigo_comuna=unique(df_poblacion$codigo_comuna),
                        year=2017:2019)
  df_def <- df_def %>% 
    left_join(df_all) %>% 
    left_join(df_allCauses) %>% 
    left_join(df_cardio) %>% 
    left_join(df_pulmonar) %>% 
    left_join(df_cardioPulmonar) %>% 
    left_join(df_cancer) %>% 
    left_join(df_extCauses)
  
  ## Add average of years, ignoring NA
  df_def_anual <- df_def %>% group_by(codigo_comuna) %>% 
    summarise(def_total=mean(def_total, na.rm=T) %>% round(0),
              def_allCauses=mean(def_allCauses, na.rm=T) %>% round(0),
              def_cardio=mean(def_cardio, na.rm=T) %>% round(0),
              def_pulmonar=mean(def_pulmonar, na.rm=T) %>% round(0),
              def_cardioPulmonar=mean(def_cardioPulmonar, na.rm=T) %>% round(0),
              def_cancer=mean(def_cancer, na.rm=T) %>% round(0),
              def_extCauses=mean(def_extCauses, na.rm=T) %>% round(0)) %>% 
    ungroup() %>% 
    mutate(year=999)
  df_def <- rbind(df_def,df_def_anual)
  
  # Add edad
  df_def <- df_def %>% mutate(edad_mayor=e)
  
  df_def_edad <- rbind(df_def_edad, df_def)
  rm(df_deis_total_filter, df_def_anual, e, df_all, df_allCauses, 
     df_cardio,df_pulmonar, df_cardioPulmonar,df_cancer,df_extCauses)
}
rm(edades_mayor)

## Expand year to columns ----------
# Gather first, then spread
df_def <- df_def_edad %>% 
  gather(cause, def,-codigo_comuna,-year,-edad_mayor) %>% 
  replace_na(list(def=0)) %>% 
  mutate(key=paste(cause,year,edad_mayor,sep="_") %>% 
           str_remove("_999") %>% 
           str_remove("_0")) %>% 
  select(-cause, -year,-edad_mayor) %>% 
  spread(key, def)


## Dataframe to merge with Datos comunales----
df_def_total <- df_def
df_def %>% names()
df_def <- df_def %>% 
  select(codigo_comuna, 
         def_total, def_total_30, def_total_65,def_total_75,
         def_allCauses, def_allCauses_30, def_allCauses_65,def_allCauses_75,
         def_extCauses, def_extCauses_30, def_extCauses_65,def_extCauses_75,
         def_cancer, def_cancer_30, def_cancer_65,def_cancer_75,
         def_pulmonar, def_pulmonar_30, def_pulmonar_65,def_pulmonar_75,
         def_cardio, def_cardio_30, def_cardio_65,def_cardio_75,
         def_cardioPulmonar, def_cardioPulmonar_30, def_cardioPulmonar_65,def_cardioPulmonar_75)

df_def$def_total %>% sum()
# Clean WS
rm(df_deis_total,df_def_edad)


## EoF