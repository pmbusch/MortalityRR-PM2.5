### MortalityRR-PM2.5
## Aggregate Death data to commune level, with details of sex, age and cause of death
## PBH Nov 2020

# Load Data ---------
options(dplyr.summarise.inform=FALSE)
source("Scripts/Aggregate_Data/deis_functions_agg.R")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

## Explore data -------
df_deis %>% names()
# df_deis <- df_deis %>% filter(year %in% c(2017:2019))
df_deis$year %>% table()

df_deis %>% group_by(cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_cap_diag1) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(cap_diag1,grupo_diag1) %>% 
  summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(cap_diag1,grupo_diag1,subcateg_diag1) %>% 
  summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_grupo_diag1) %>% summarise(count=n()) %>% arrange(desc(count))

df_deis %>% group_by(cap_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_cap_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(grupo_diag2) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis %>% group_by(glosa_grupo_diag2) %>% summarise(count=n()) %>% arrange(desc(count))




## Grupos de interes
## All: Todas las filas
## All causes: Causas de morbilidad y mortalidad. A00-Q99 is.na(cap_diag2)
df_deis %>% filter(is.na(cap_diag2)) %>% nrow()
## Cardio: Enfermedades del sistema circulatorio. All I
df_deis %>% filter(str_detect(subcateg_diag1,"I")) %>% nrow()
df_deis %>% filter(str_detect(subcateg_diag1,"I")) %>% pull(cap_diag1) %>% unique()
## Pulmonar: Enfermedades del sistema respiratorio. All J
df_deis %>% filter(str_detect(subcateg_diag1,"J")) %>% nrow()
df_deis %>% filter(str_detect(subcateg_diag1,"J")) %>% pull(cap_diag1) %>% unique()
## Cardiopulmonar: Suma cardio+pulmonar. All I and J
df_deis %>% filter(str_detect(subcateg_diag1,"I|J")) %>% nrow()
df_deis %>% filter(str_detect(subcateg_diag1,"I|J")) %>% pull(cap_diag1) %>% unique()
## Cancer: Tumores [Neoplasias]. All C
df_deis %>% filter(str_detect(subcateg_diag1,"C")) %>% nrow()
df_deis %>% filter(str_detect(subcateg_diag1,"C")) %>% pull(grupo_diag1) %>% unique()
## Lung Cancer
df_deis %>% filter(categ_diag1 %in% c("C34","C33")) %>% nrow()
## External causes: Suplement of all causes
df_deis %>% filter(!is.na(cap_diag2)) %>% nrow()
## Suicides
df_deis %>% filter(grupo_diag2=="X60-X84") %>% nrow()

# Explore external causes
df_deis %>% names()
df_deis %>% filter(!is.na(cap_diag2)) %>% group_by(glosa_categ_diag2) %>% 
  summarise(count=n()) %>% arrange(desc(count))

# df_deis %>% filter(!is.na(cap_diag2)) %>% 
#   write.table("externalDeaths.csv", sep=";",row.names = F)


# Age
df_deis$edad_tipo %>% unique()
df_deis %>% group_by(edad) %>% summarise(count=n()) %>% arrange(desc(count))

## GET ADJUSTED MORTALITY RATE --------------------------

## Function to get the specific causes of death of interest into a common dataframe
f_adjMR_causes <- function(common_df,
                           df_death,df_pop,
                           sex_filter2=T,name_end=""){
  
  # All Causes
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = is.na(cap_diag2),
                       name_var = paste("mrAdj_AllCauses",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  # Cardiopulmonary
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = str_detect(subcateg_diag1,"I|J"),
                       name_var = paste("mrAdj_CDP",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  ## CVD
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = str_detect(subcateg_diag1,"I"),
                       name_var = paste("mrAdj_CVD",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  
  # Respiratory
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = str_detect(subcateg_diag1,"J"),
                       name_var = paste("mrAdj_RSP",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  
  # Cancer
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = str_detect(subcateg_diag1,"C"),
                       name_var = paste("mrAdj_CAN",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  # Lung Cancer
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = categ_diag1 %in% c("C34","C33"),
                       name_var = paste("mrAdj_LCA",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  # External causes
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = !is.na(cap_diag2),
                       name_var = paste("mrAdj_ExtCauses",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  # Suicidal: Lesiones autoinfligidas intencionalmente 
  mrAdj_aux <- f_adjMR(df_deis,df_population_deis,
                       cause_filter = grupo_diag2=="X60-X84",
                       name_var = paste("mrAdj_SUI",name_end,sep=""),
                       sex_filter = {{sex_filter2}})
  common_df <- left_join(common_df, mrAdj_aux, by=c("codigo_comuna")); rm(mrAdj_aux)
  
  return(common_df)
}


# Use all communes as base
mrAdj <- data.frame(codigo_comuna=unique(df_population_deis$codigo_comuna))

# All Gender
mrAdj <- f_adjMR_causes(mrAdj, df_deis, df_population_deis)
# Male
mrAdj <- f_adjMR_causes(mrAdj, df_deis, df_population_deis, name_end = "_male",
                        sex_filter2 = sexo %in% c("Hombre","hombre"))
# Female
mrAdj <- f_adjMR_causes(mrAdj, df_deis, df_population_deis, name_end = "_female",
                        sex_filter2 = sexo %in% c("Mujer","mujer"))


### GET MORTALITY RATES --------------------

## Function to get the specific causes of death of interest into a common dataframe
f_MR_causes <- function(common_df,
                           df_death,df_pop,
                           sex_filter2=T, age_filter2=T,
                           name_end=""){
  
  # All Causes
  mr_aux <- f_MR(df_deis,df_population_deis,
                 cause_filter = is.na(cap_diag2),
                 name_var = paste("mr_AllCauses",name_end,sep=""),
                 sex_filter = {{sex_filter2}},
                 age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  # Cardiopulmonary
  mr_aux <- f_MR(df_deis,df_population_deis,
                    cause_filter = str_detect(subcateg_diag1,"I|J"),
                    name_var = paste("mr_CDP",name_end,sep=""),
                    sex_filter = {{sex_filter2}},
                    age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  ## CVD
  mr_aux <- f_MR(df_deis,df_population_deis,
                    cause_filter = str_detect(subcateg_diag1,"I"),
                    name_var = paste("mr_CVD",name_end,sep=""),
                    sex_filter = {{sex_filter2}},
                    age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  
  # Respiratory
  mr_aux <- f_MR(df_deis,df_population_deis,
                    cause_filter = str_detect(subcateg_diag1,"J"),
                    name_var = paste("mr_RSP",name_end,sep=""),
                    sex_filter = {{sex_filter2}},
                    age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  
  # Cancer
  mr_aux <- f_MR(df_deis,df_population_deis,
                    cause_filter = str_detect(subcateg_diag1,"C"),
                    name_var = paste("mr_CAN",name_end,sep=""),
                    sex_filter = {{sex_filter2}},
                    age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  # Lung Cancer
  mr_aux <- f_MR(df_deis,df_population_deis,
                 cause_filter = categ_diag1 %in% c("C34","C33"),
                 name_var = paste("mr_LCA",name_end,sep=""),
                 sex_filter = {{sex_filter2}},
                 age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  # External Causes
  mr_aux <- f_MR(df_deis,df_population_deis,
                 cause_filter = !is.na(cap_diag2),
                 name_var = paste("mr_ExtCauses",name_end,sep=""),
                 sex_filter = {{sex_filter2}},
                 age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  # Suicide: Lesiones autoinfligidas intencionalmente 
  mr_aux <- f_MR(df_deis,df_population_deis,
                 cause_filter = grupo_diag2=="X60-X84",
                 name_var = paste("mr_SUI",name_end,sep=""),
                 sex_filter = {{sex_filter2}},
                 age_filter={{age_filter2}})
  common_df <- left_join(common_df, mr_aux, by=c("codigo_comuna")); rm(mr_aux)
  
  return(common_df)
}


# Function to get Causes and age gruoups: 
# All, 30+, 65+, 75+, 0-30, 30-64, 65-74
f_MR_causes_Age <- function(common_df,
                            df_death,df_pop,
                            sex_filter3=T,
                            name_end2=""){
  # All ages
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_allAges",sep=""))
  # 30+
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_30plus",sep = ""),
                           age_filter2 = !(age_group %in% c("0-4","5-9","10-14",
                                                            "15-19","20-24","25-29")))
  # 65+
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_65plus",sep = ""),
                           age_filter2 = age_group %in% c("65-69","70-74","75-79","80+"))
  # 75+
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_75plus",sep = ""),
                           age_filter2 = age_group %in% c("75-79","80+"))
  
  # 0-30
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_0_30",sep = ""),
                           age_filter2 = age_group %in% c("0-4","5-9","10-14",
                                                            "15-19","20-24","25-29"))
  
  # 30-64
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_30_64",sep = ""),
                           age_filter2 = age_group %in% c("30-34","35-39","40-44",
                                                          "45-49","50-54","55-59",
                                                          "60-64"))
  
  # 65-74
  common_df <- f_MR_causes(common_df, df_deis, df_population_deis,
                           sex_filter2 = {{sex_filter3}},
                           name_end = paste(name_end2,"_65_74",sep = ""),
                           age_filter2 = age_group %in% c("65-69","70-74"))
  
}
  
# Use all communes as base
mr <- data.frame(codigo_comuna=unique(df_population_deis$codigo_comuna))

# All gender
mr <- f_MR_causes_Age(mr, df_deis, df_population_deis)
# Male
mr <- f_MR_causes_Age(mr, df_deis, df_population_deis, name_end2 = "_male",
                      sex_filter3 = sexo %in% c("Hombre","hombre"))
# Female
mr <- f_MR_causes_Age(mr, df_deis, df_population_deis, name_end2 = "_female",
                      sex_filter3 = sexo %in% c("Mujer","mujer"))


## Summary ----------
# mrAdj %>% skim()
# mr %>% skim()

rm(df_population_deis)

## EoF