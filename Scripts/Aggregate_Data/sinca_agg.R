### MortalityRR-PM2.5
## Aggregate air pollution data
## PBH Dec 2020

## FIXED RADIUS ------
# Load hourly monitor data: scrapped from SINCA --------
# Population data
file_name <- "Figures/ExposureEstimation/%s.%s"
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/population_agg.R", encoding = "UTF-8")
df <- read_rds("Data/Data_Model/AirPollution_Data_raw.rsd")

df %>% group_by(pollutant, unidad) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

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
  group_by(codigo_comuna, pollutant, unidad, site, 
           longitud, latitud,year,season) %>% 
  summarise(
    max_value=max(valor,na.rm=T), min_value=min(valor,na.rm=T),
    valor=mean(valor, na.rm=T),
    disponibilidad=n()/365) %>% ungroup()

# At least 80% days in the year with data, 
# and the three years with data (2017-2019)
valid_sites <- df_conc %>% 
  filter(season=="anual" & disponibilidad>0.8 & pollutant=="mp2.5") %>% 
  group_by(codigo_comuna,site) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% pull(site)
valid_sites %>% length()

df_conc <- df_conc %>% 
  filter(site %in% valid_sites) %>% 
  group_by(codigo_comuna, site,longitud, latitud, season, pollutant) %>% 
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

## Weighted avg per population for each commune
df_mp_pop <- df_mp %>% 
  group_by(codigo_comuna, season, pollutant) %>% 
  summarise(valor=weighted.mean(valor, poblacion)) %>% ungroup() %>% 
  mutate(season=paste(pollutant,season,sep="_") %>% str_remove_all("_anual|\\.")) %>% 
  select(-pollutant) %>% spread(season,valor) %>% 
  right_join(map_commune) %>% left_join(codigos_territoriales)

df_mp_pop %>% filter(!is.na(mp25)) %>% nrow() # N commune:120

# MAPS  ------
library(RColorBrewer)
library(mapview)
library(leaflet)

## Monitors, communes and district zones ----
estaciones_sinca <- df_conc %>% 
  group_by(site, codigo_comuna, longitud, latitud) %>% 
  summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
  na.omit() %>% 
  left_join(codigos_territoriales)

estaciones_sinca <- st_as_sf(estaciones_sinca, 
                             coords = c("longitud","latitud"),
                             remove = F, 
                             crs="+proj=longlat +ellps=GRS80 +no_defs")


m_sinca <- mapview(estaciones_sinca, label=estaciones_sinca$site, col.regions="red",
        layer.name = c("PM2.5 Monitoring Sites"))

m_commune <- map_commune %>% 
  left_join(codigos_territoriales) %>% 
  mapview(layer.name = c("Communes"),
          label=.$nombre_comuna,
          lwd=4,
          alpha.regions=0.1, col.regions="green")

m_zonas <- mapa_zonas %>% st_as_sf() %>% 
  left_join(codigos_territoriales) %>% 
  mapview(label=.$nombre_comuna,
          layer.name = c("District census zones"),
          lwd=2.5,
          alpha.regions=0.1, col.regions="purple")

mapshot(m_sinca+m_commune+m_zonas,
        sprintf(file_name,"Polygons_sites","html"),
        selfcontained=F)
  

## Buffer and centroids ----
zonas <- rmapshaper::ms_dissolve(mapa_zonas %>% st_as_sf(), 
                                 field = "codigo_comuna") %>%
  left_join(codigos_territoriales)
zonas_centroide <- zonas %>% as.data.frame() %>% 
  mutate(centroide=st_centroid(geometry),
         cent_lon=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  st_as_sf(coords = c("cent_lon","cent_lat"),
           remove = F, 
           crs="+proj=longlat +ellps=GRS80 +no_defs")

m_commune_centroid <- mapview(zonas_centroide, label=zonas_centroide$nombre_comuna,
        layer.name = c("District census zones"),
        col.regions="blue")

# https://stackoverflow.com/questions/60895518/why-is-st-buffer-function-not-creating-an-r-object-that-correctly-displays-in-ma
dist_buffer <- 20
# EPSG:5361 = SIRGAS-Chile 2002 / UTM zone 19S

buffer_sinca <- estaciones_sinca %>% st_transform(5361) %>% 
  st_buffer(dist_buffer*1e3) %>% 
  st_transform(4326)

m_buffer <- mapview(buffer_sinca, label=buffer_sinca$site, col.regions="red",
                    layer.name = c("Buffer 20km"),alpha.regions=0.1 )


mapshot(m_sinca+m_commune+m_zonas+m_buffer+m_commune_centroid,
        sprintf(file_name,"Buffer","html"),
        selfcontained=F)


## PM2.5 at district zones -----
pm25_zones <- left_join(mapa_zonas, df_mp, by=c("geocodigo")) %>% st_as_sf()
m_pm25_zones <- pm25_zones %>% filter(season=="anual") %>%
  mapview(zcol="valor",
          layer.name = c("PM2.5 [ug/m3]"),
          at=seq(20,40,2),
          # alpha.regions=0.3,
          col.regions=brewer.pal(11, "YlOrRd"))

mapshot(m_sinca+m_pm25_zones,
        sprintf(file_name,"PM25_District","html"),
        selfcontained=F)

## PM2.5 exposure at communes -----
m_pm25_commune <- df_mp_pop %>% st_as_sf() %>% 
  filter(!is.na(mp25)) %>% 
  mapview(
    label=paste(.$nombre_comuna,": ",
                round(.$mp25,2),"[ug/m3]",sep=""),
    layer.name="PM2.5 Exposure [ug/m3]",
    zcol="mp25",
    na.color="white",
    at=seq(20,40,2),
    # alpha.regions=0.3,
    col.regions=brewer.pal(11, "YlOrRd"))

mapshot(m_sinca+m_pm25_commune, sprintf(file_name,"PM25_Commune","html"), 
        selfContained=F)


# Save data ----------
df_mp_pop %>% names()
df_conc_save <- df_mp_pop %>% 
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