### MortalityRR-PM2.5
## Aggregate air pollution data
## PBH Dec 2020

## FIXED RADIUS ------
# Load hourly monitor data: scrapped from SINCA --------
# Population data
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/population_agg.R", encoding = "UTF-8")
df <- read_rds("Data/Data_Model/AirPollution_Data_raw.rsd")

df %>% group_by(pollutant, unidad) %>% summarise(count=n()) %>% arrange(desc(count))

df %>% filter(pollutant=="mp2.5") %>% pull(valor) %>% range()
df %>% group_by(tipo_dato) %>% summarise(count=n()) %>% arrange(desc(count))


## Expand to other years and seasons ---------
## Special year (avg 2017-2019)
df <- df %>%
  filter(year %in% 2017:2020 & pollutant %in% c("mp2.5","mp10")) %>%
  mutate(season=as.character(year))
df$season %>% unique()

# Df with average per year
df_anos <- df
df <- df %>% filter( year %in% 2017:2019) %>% mutate(season=NULL)

## Add Season (avg 2017-2019)
df %>% names() %>% sort()
df <- df %>% mutate(season=getSeason(date))
## Add anual: promedio 2017-2019
df <- df %>% mutate(season="anual") %>% rbind(df)
# Add other years as season
df <- df %>% rbind(df_anos)
df$season %>% unique(); df$pollutant %>% unique()
rm(df_anos)

# Avg of concentracion
df_conc <- df %>% 
  group_by(codigo_comuna, pollutant, unidad, site,year,season) %>% 
  summarise(
    max_value=max(valor,na.rm=T), min_value=min(valor,na.rm=T),
    valor=mean(valor, na.rm=T),
    disponibilidad=n()/365) %>% ungroup()

# At least 80% days in the year with data, and the three years with data (2017-2019)
valid_sites <- df_conc %>% 
  filter(season=="anual" & disponibilidad>0.8 & pollutant=="mp2.5") %>% 
  group_by(codigo_comuna,site) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% pull(site)


df_conc <- df_conc %>% 
  filter(site %in% valid_sites) %>% 
  group_by(codigo_comuna, site, season, pollutant) %>% 
  summarise( max_value=max(valor,na.rm=T), min_value=min(valor,na.rm=T),
             valor=mean(valor, na.rm=T)) %>% ungroup()
rm(valid_sites)
df_conc$site %>% unique()

# Parameters in ranges
df_conc$max_value %>% range()
df_conc$min_value %>% range()
df_conc$valor %>% range(na.rm=T)

## Fixed Radius: Distances
fixed_rad <- 20

## Load distance of monitors with centroid of communes and district zones
df_dist <- read_rds("Data/Data_Model/distanceCommuneMonitor.rsd")
df_dist_zona <- read_rds("Data/Data_Model/distanceZoneMonitor.rsd")

# Filter only communes within the radious of a monitor
# Note: Centroid of each commune is estimated using the disctrict zones, where people live
df_dist <- df_dist %>% filter(dist<fixed_rad*1e3)

# Communes and monitors within this radius
comunas_site <- df_dist %>% 
  mutate(tupla=paste(codigo_comuna,site,sep="-")) %>% 
  pull(tupla) %>% unique()

# Filter of district zones: tuples commune-monitor available
df_dist_zona <- df_dist_zona %>% 
  mutate(tupla=paste(codigo_comuna,site,sep="-")) %>% 
  filter(tupla %in% comunas_site) %>% 
  select(geocodigo, codigo_comuna, nombre_comuna, site, dist)

## Add data population
censo_2017_zonas$poblacion %>% sum()
pob <- censo_2017_zonas %>% group_by(geocodigo) %>% 
  summarise(poblacion=sum(poblacion, na.rm=T)) %>% ungroup()
df_avg <- df_dist_zona %>% left_join(pob, by=c("geocodigo")) %>% na.omit()
rm(pob)

# Add conc data
df_avg <- df_avg %>% left_join(df_conc %>% select(-codigo_comuna), 
                               by=c("site")) %>% filter(!is.na(valor))

# Weighted avg. by the inverse of the distance to each district zone
df_mp <- df_avg %>% 
  group_by(geocodigo, codigo_comuna, poblacion, season, pollutant) %>% 
  summarise(valor=weighted.mean(valor, 1/(dist)),
            count=n()) %>% ungroup()

# # View map on zonas
# library(RColorBrewer)
# m2 <- left_join(mapa_zonas, df_mp, by=c("geocodigo")) %>% st_as_sf()
# m3 <- m2 %>% filter(season=="anual") %>% 
#   mapview(zcol="valor",col.regions=brewer.pal(9, "YlOrRd"))
# mapshot(m3, "Figuras/ConcentracionMP25_zonas.html",
#         selfcontained=F)
# rm(m2,m3)

## Weighted avg per population for each commune
df_mp <- df_mp %>% 
  group_by(codigo_comuna, season, pollutant) %>% 
  summarise(valor=weighted.mean(valor, poblacion)) %>% ungroup() %>% 
  mutate(season=paste(pollutant,season,sep="_") %>% str_remove_all("_anual|\\.")) %>% 
  select(-pollutant) %>% spread(season,valor) %>% 
  right_join(map_commune) %>% left_join(codigos_territoriales)

df_mp %>% filter(!is.na(mp25)) %>% nrow() # N commune:120

# m1 <- mapview(df_mp %>% st_as_sf(), 
#       label=paste(df_mp$nombre_comuna,": ",
#                   round(df_mp$mp25,2),"[ug/m3]",sep=""),
#       layer.name="Concentracion MP2.5 2016-2019",
#       zcol="mp25",
#       na.color="white",
#       col.regions=brewer.pal(9, "YlOrRd"))
# m1
# mapshot(m1, "Figuras/ConcentracionMP25.html", selfContained=F)
# rm(m1)

# Save data ----------
df_mp %>% names()
df_conc_save <- df_mp %>% 
  select(codigo_comuna, mp25, mp25_fall, mp25_winter, mp25_spring, mp25_summer,
         mp25_2017, mp25_2018, mp25_2019, mp25_2020,
         mp10, mp10_fall, mp10_winter, mp10_spring, mp10_summer,
         mp10_2017, mp10_2018, mp10_2019, mp10_2020) %>% 
  filter(!is.na(mp25))
saveRDS(df_conc_save, "Data/Data_Model/AirPollution_Data_20km.rsd")


## Alternativ method: Monitor in each commune ------------
# Load data --------
# source("Scripts/Load_Data/sinca_scrap.R", encoding = "UTF-8") # Baja los datos
df <- read_rds("Data/Data_Model/AirPollution_Data_raw.rsd")

## Agregar a nivel comunal -----------
# Promedio 2017-2019
df_conc <- df %>% 
  filter(year %in% 2017:2019 & pollutant=="mp2.5") %>% 
  group_by(codigo_comuna, pollutant, unidad, site,year) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup()
# Disponibildiad mayor a 80% y estaciones con todos los años de datos
df_conc <- df_conc %>% 
  filter(disponibilidad>0.8) %>% 
  group_by(site,codigo_comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% select(-count)

# Numero estaciones
df_conc %>% n_distinct("site")

# Promedio por comuna
df_conc <- df_conc %>%   
  group_by(codigo_comuna, pollutant, unidad) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup()

# Numero comunas
df_conc %>% nrow()

# Resumir data
df_conc <- df_conc %>% 
  rename(mp25=valor) %>% 
  select(codigo_comuna, mp25)

# Guarda datos ----------
saveRDS(df_conc, "Data/Data_Model/AirPollution_Data_Commune.rsd")

rm(df)
## EoF