### MortalityRR-PM2.5
## Load population, commune code and polygon for maps
## PBH Nov 2020


## Load population --------------
# Library chilemapas
df_population <- left_join(censo_2017_comunas, codigos_territoriales)
df_population %>% names()
df_population$poblacion %>% sum()

## Maps ---------
## Commune --------------
# Remove from map "Isla de Pascua" and "Juan Fernandez"
map_commune <- mapa_comunas %>% st_as_sf() %>% 
  filter(!(codigo_comuna %in% c("05201","05104"))) %>% 
  mutate(superficie=st_area(geometry) %>% as.numeric(),
         perimetro=st_length(geometry) %>% as.numeric())

# Levels region
levels_region <- c("XV","I","II","III","IV","V","M","VI","VII","VIII",
                   "IX","XIV","X","XI","XII")


## Create useful variables for regional data
dicc_region <- codigos_territoriales %>% 
  select(codigo_region,nombre_region) %>% 
  distinct() %>% 
  mutate(cod_region=case_when(
    codigo_region=="01" ~ "I",
    codigo_region=="02" ~ "II",
    codigo_region=="03" ~ "III",
    codigo_region=="04" ~ "IV",
    codigo_region=="05" ~ "V",
    codigo_region=="06" ~ "VI",
    codigo_region=="07" ~ "VII",
    codigo_region=="08" ~ "VIII",
    codigo_region=="09" ~ "IX",
    codigo_region=="10" ~ "X",
    codigo_region=="11" ~ "XI",
    codigo_region=="12" ~ "XII",
    codigo_region=="13" ~ "M",
    codigo_region=="14" ~ "XIV",
    codigo_region=="15" ~ "XV",
    codigo_region=="16" ~ "VIII",
    T ~ "otro") %>% factor(levels = levels_region)) %>% 
  mutate(region=nombre_region %>% 
      str_remove_all("Libertador General Bernardo | del General Carlos Ibanez del Campo| y de la Antartica Chilena| de Santiago| y Parinacota") %>% 
      factor(levels=c("Arica","Tarapaca","Antofagasta","Atacama","Coquimbo",
                      "Valparaiso","Metropolitana","OHiggins","Maule",
                      "Biobio","Nuble","La Araucania","Los Rios","Los Lagos","Aysen",
                      "Magallanes"))) %>% 
  mutate(zone=case_when(
    cod_region %in% c("XV","I","II","III","IV") ~ "North",
    cod_region %in% c("V","VI","VII") ~ "Center",
    cod_region %in% c("M") ~ "Metropolitan",
    cod_region %in% c("VIII","XVI","IX","XIV") ~ "South",
    cod_region %in% c("X","XI","XII") ~ "Patagonia") %>% 
      factor(levels=c("North","Center","Metropolitan","South","Patagonia")))
  
map_commune <- map_commune %>% left_join(dicc_region, by=c("codigo_region"))




## Boolean variable for map of Santiago zone
# All communs for the province of Santiago, minus Lo Barnechea
# Additional communes:  San Bernardo, Puente Alto, Padre Hurtado, calera de tango, 
map_commune <- map_commune %>% 
  mutate(map_rm=if_else((codigo_provincia=="131" & codigo_comuna!="13115")|
                           codigo_comuna %in% c("13201","13401","13403","13604"),
                         1,0))

## Region ------------
map_region <- generar_regiones(mapa_comunas %>% filter(codigo_comuna!="05201" &
                                                       codigo_comuna!="05104"))
map_region$codigo_region %>% unique()
map_region <- map_region %>% left_join(dicc_region, by=c("codigo_region"))


## Census area per commune ----
## Note: there are zones without map, so there is no area
censo_2017_comunas$poblacion %>% sum()
censo_2017_zonas$poblacion %>% sum()
cat(sum(censo_2017_zonas$poblacion)/sum(censo_2017_comunas$poblacion)*100,
    " % of population in census districts")

# Area per district
df_zona <- mapa_zonas %>%
  left_join(codigos_territoriales) %>%
  mutate(superficie=st_area(geometry) %>% as.numeric(),
         perimetro=st_length(geometry) %>% as.numeric())

# Area of census district per commune
df_zona <- df_zona %>% group_by(codigo_comuna) %>%
  summarise(superficie_censal=sum(superficie, na.rm=T)) %>% ungroup() %>%
  right_join(map_commune) %>%
  mutate(perc_censal=superficie_censal/superficie*100)
# Note: some values are above 100% (ej: 100.2). Could  be due to approx. errors


# Add area to commune map
map_commune <- map_commune %>% 
  left_join(df_zona %>% select(codigo_comuna,superficie_censal)) %>% 
  mutate(superficie_censal=if_else(is.na(superficie_censal),
                                   superficie,superficie_censal))

rm(df_zona)

## EoF