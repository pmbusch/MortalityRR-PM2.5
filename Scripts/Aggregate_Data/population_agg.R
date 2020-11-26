### MortalityRR-PM2.5
## Aggregate population at commune level
## PBH Nov 2020

# Load map data with codes and population--------
source("Scripts/Load_Data/map_load.R", encoding = "UTF-8")

# Create Age group
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
    T ~ "80+"))
# df_population %>% group_by(edad, age_group) %>% summarise(count=n()) %>%
#   arrange(desc(count)) %>% view()


# Create major age groups
df_population <- df_population %>% 
  mutate(major_age_group=case_when(
    age_group %in% c("0-4","5-9","10-14") ~ "0-14",
    age_group %in% c("15-19","20-24","25-29","30-34",
                     "35-39","40-44") ~ "15-44",
    age_group %in% c("45-49","50-54","55-59","60-64") ~ "45-64",
    T ~ "65+"))
df_population %>% group_by(major_age_group,age_group) %>% summarise(count=n())

## Summarise data as major group age percentages
df_age <- df_population %>% 
  mutate(major_age_group=paste("age_",major_age_group,sep="") %>% 
           str_replace_all("-","_") %>% 
           str_replace_all("\\+","plus")) %>% 
  group_by(codigo_comuna, major_age_group) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(perc_age=pob/sum(pob)*100,
         pob=NULL) %>% ungroup() %>% 
  # filter(major_age_group!="0-14") %>%
  spread(major_age_group,perc_age)

# Add 30+
df_age_30plus <- df_population %>% 
  mutate(bool_aux=age_group %in% c("30-34","35-39","40-44",
                                   "45-49","50-54","55-59","60-64",
                                   "65-69", "70-74","75-79","80+")) %>% 
  group_by(codigo_comuna,bool_aux) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% 
  mutate(age_30plus=100*poblacion/sum(poblacion, na.rm=T)) %>% ungroup() %>% 
  filter(bool_aux==T) %>% select(codigo_comuna, age_30plus)

# Add 75+
df_age_75plus <- df_population %>% 
  mutate(bool_aux=age_group %in% c("75-79","80+")) %>% 
  group_by(codigo_comuna,bool_aux) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% 
  mutate(age_75plus=100*poblacion/sum(poblacion, na.rm=T)) %>% ungroup() %>% 
  filter(bool_aux==T) %>% select(codigo_comuna, age_75plus)

# Join
df_age <- df_age %>% 
  left_join(df_age_30plus, by = c("codigo_comuna")) %>% 
  left_join(df_age_75plus, by = c("codigo_comuna"))



## SEX -----------
df_sex <- df_population %>% 
  group_by(codigo_comuna, sexo) %>% 
  summarise(pob=sum(poblacion,na.rm=T)) %>% 
  mutate(porc=pob/sum(pob)*100,
         pob=NULL) %>% ungroup() %>% 
  filter(sexo!="hombre") %>% spread(sexo,porc) %>%
  rename(perc_female=mujer)


## Population per commune ----------
df_population <- df_population %>% 
  group_by(codigo_comuna) %>% 
  summarise(population=sum(poblacion,na.rm=T)) %>% ungroup()
df_population$population %>% sum()


# Join all -----
df_population <- left_join(df_population, df_age) %>% 
  left_join(df_sex) %>% 
  left_join(codigos_territoriales)

# Clean WS
rm(df_age,df_sex, df_age_30plus, df_age_75plus)


## Add area data----------
df_population <- df_population %>% 
  left_join(map_region %>% select(codigo_region, region)) %>% 
  select(-geometry) %>% 
  left_join(map_commune %>% 
              select(codigo_comuna, superficie, perimetro,superficie_censal,
                     zone, map_rm))

# # Curva acumulada poblacion ------
# theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# df_population %>%
#   arrange(desc(population)) %>%
#   rowid_to_column() %>%
#   ggplot(aes(x=reorder(rowid,desc(population)),y=cumsum(population), group=1))+
#   geom_line(size=1)+
#   labs(x="# Comunas", y="Acc. population")+
#   scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
#   scale_x_discrete(breaks=seq(0,350,50))
# 
# source("Scripts/00-Funciones.R", encoding = "UTF-8")
# f_savePlot(last_plot(),"Figuras/Analisis_general/PobAcumulada.png",dpi=300)

## Urban density based on disctric zones -----------
# Some statistics using area from district zones and population
df_zona <- censo_2017_zonas %>%
  left_join(mapa_zonas) %>% 
  left_join(codigos_territoriales) %>%
  mutate(superficie=st_area(geometry) %>% as.numeric(),
         perimetro=st_length(geometry) %>% as.numeric()) %>% 
  na.omit()
df_zona$poblacion %>% sum()

# Density
df_zona <- df_zona %>% 
  mutate(urbanDensity=poblacion/superficie*1e6)

## Agrupar a nivel de comuna
df_zona <- df_zona %>% 
  group_by(codigo_comuna) %>% 
  summarise(urbanDensity_mean=mean(urbanDensity, na.rm=T),
            urbanDensity_median=median(urbanDensity, na.rm=T),
            urbanDensity_mean_p90=quantile(urbanDensity,0.9, na.rm=T)) %>% 
  ungroup()

## Add to df_poblacion
df_population <- df_population %>% left_join(df_zona) %>% 
  mutate(density=population/superficie*1e6,
         urbanDensity=poblacion/superficie_censal*1e6)
rm(df_zona)


## EoF