### MortalityRR-PM2.5
## Functions to get MR (mortality rates) from Deis data
## PBH Nov 2020

# Load Data ---------
source("Scripts/Load_Data/deis_load.R", encoding = "UTF-8")
# source("Scripts/Load_Data/map_load.R", encoding = "UTF-8")
# source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

## Note: Gets MR using data from deaths 2017-2019 and population from 2019

## Adjusted MR ----------------
# MR adjusted to age groups.

# Function to get adjusted MR rates
# Uses population data from Census 2017
f_adjMR <- function(deaths, population, cause_filter=T, sex_filter=T, name_var="MR_adj"){

  ## Get national and communes age profiles
  national_age_profile <- population %>% 
    filter({{sex_filter}}) %>% 
    group_by(age_group) %>% 
    summarise(pop_national=sum(poblacion,na.rm=T)) %>% ungroup() %>% 
    mutate(perc_pop=pop_national/sum(pop_national))
  # national_age_profile$pop_national %>% sum()
  # national_age_profile$perc_pop %>% sum()
  
  commune_age_profile <- population %>% 
    filter({{sex_filter}}) %>% 
    group_by(codigo_comuna,age_group) %>% 
    summarise(pop=sum(poblacion,na.rm=T)) %>% ungroup()
  # commune_age_profile$pop %>% sum()
  
  # Apply filter to death data: Divided by 3 because we have 3 years of data
  deaths <- deaths %>% 
    filter({{cause_filter}}) %>% 
    filter({{sex_filter}}) %>% 
    group_by(codigo_comuna, age_group) %>% 
    summarise(total_deaths=n()/3) %>% ungroup()
    # replace_na(list(total_deaths=0))

  ## Join with age group
  deaths <- deaths %>% 
    left_join(commune_age_profile, by = c("codigo_comuna", "age_group"))
  
  # Calculate MR
  df_mr <- deaths %>% 
    mutate(mr=if_else(pop==0, 0, total_deaths/pop*1e5))
  
  # Ajusted MR with national age profile
  df_mr <- df_mr %>% left_join(national_age_profile, by = c("age_group")) %>% 
    mutate(mr_stand=mr*perc_pop)
  
  df_mr <- df_mr %>% 
    group_by(codigo_comuna) %>% 
    summarise(mr_stand=sum(mr_stand, na.rm=T)) %>% ungroup()
  
  # Change name
  names(df_mr) <- c("codigo_comuna", name_var)
  
  return(df_mr)
}

## Normal MR ----------------
# Function to get adjusted MR rates
# Uses population data from Census 2017
f_MR <- function(deaths, population, 
                 cause_filter=T, age_filter=T, sex_filter=T, 
                 name_var="MR"){
  
  ## Get communes population
  commune_population <- population %>% 
    filter({{sex_filter}}) %>% 
    filter({{age_filter}}) %>% 
    group_by(codigo_comuna) %>% 
    summarise(pop=sum(poblacion,na.rm=T)) %>% ungroup()
  
  # Apply filter to death data: Divided by 3 because we have 3 years of data
  deaths <- deaths %>% 
    filter({{cause_filter}}) %>% 
    filter({{age_filter}}) %>% 
    filter({{sex_filter}}) %>% 
    group_by(codigo_comuna) %>% 
    summarise(total_deaths=n()/3) %>% ungroup()
  
  
  ## Join with population
  deaths <- deaths %>% 
    left_join(commune_population, by = c("codigo_comuna"))
  
  # Calculate MR
  df_mr <- deaths %>% 
    mutate(mr=total_deaths/pop*1e5,
           total_deaths=NULL, pop=NULL)

  # Change name
  names(df_mr) <- c("codigo_comuna", name_var)
  
  return(df_mr)
}

## EoF