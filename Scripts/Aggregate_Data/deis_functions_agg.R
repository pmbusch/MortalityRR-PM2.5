### MortalityRR-PM2.5
## Functions to get MR (mortality rates) from Deis data
## PBH Nov 2020

# Load Data ---------
source("Scripts/Load_Data/deis_load.R", encoding = "UTF-8")
source("Scripts/Load_Data/map_load.R", encoding = "UTF-8")
# source("Scripts/Aggregate_Data/poblacion_agg.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))

## Note: Gets MR using data from deaths 2017-2019 and population from 2019

## Adjusted MR ----------------
# MR adjusted to age groups.

## Create age groups
df_deis$age_group %>% unique()
# Age group
df_population$edad %>% unique()
df_population <- df_population %>% 
  mutate(age_group=case_when(
    edad %in% c("0 a 4") ~ "0-4",
    edad %in% c("5 a 9") ~ "5-9",
    edad %in% c("10 a 14") ~ "10-14",
    edad %in% c("15 a 19") ~ "15-19",
    edad %in% c("20 a 24") ~ "20-24",
    edad %in% c("25 a 29") ~ "25-29",
    edad %in% c("30 a 34") ~ "30-34",
    edad %in% c("35 a 39") ~ "35-39",
    edad %in% c("40 a 44") ~ "40-44",
    edad %in% c("45 a 49") ~ "45-49",
    edad %in% c("50 a 54") ~ "50-54",
    edad %in% c("55 a 59") ~ "55-59",
    edad %in% c("60 a 64") ~ "60-64",
    edad %in% c("65 a 69") ~ "65-69",
    edad %in% c("70 a 74") ~ "70-74",
    edad %in% c("75 a 79") ~ "75-79",
    # edad %in% c("80 a 84") ~ "80-84",
    T ~ "80+"))
# df_population %>% group_by(edad, age_group) %>% summarise(count=n()) %>% 
#   arrange(desc(count)) %>% view()


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

# all_deaths <- f_adjMR(df_deis, df_population)
# cardioPulmonar_deaths <- f_adjMR(df_deis, df_population, 
#                                  cap_diag1 %in% c("J00-J99","I00-I99"),
#                                  name_var = "CDP_all")
# cardioPulmonar_male <- f_adjMR(df_deis, df_population, cap_diag1 %in% c("J00-J99","I00-I99"),
#                                sexo %in% c("Hombre","hombre"),
#                                name_var = "CDP_male")
# 
# a <- left_join(cardioPulmonar_deaths,cardioPulmonar_male)
# 
# 
# cancer_deaths <- f_adjMR(df_deis, df_population, cap_diag1 %in% c("C00-D48"))


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

# total_deaths <- f_MR(df_deis,df_population)
# total_deaths_above30 <- f_MR(df_deis,df_population,
#                              age_filter = !(age_group %in% c("0-4","5-9","10-14","15-19",
#                                                              "20-24","25-29")))

## EoF