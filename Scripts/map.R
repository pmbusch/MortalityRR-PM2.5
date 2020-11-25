### MortalityRR-PM2.5
## Generate map figure for paper
## PBH Nov 2020

## Load -------
load(".RData")
source("Scripts/00-Functions.R", encoding = "UTF-8")
# file_name <- "Scripts/Analisis_Exploratorios/Figuras/Mapas/%s.png"
# source("Scripts/Analisis_Exploratorios/f_figuras.R", encoding = "UTF-8")


## MP2.5 + Monitor --------
## Load Distance data
df_dist <- read_rds("Data/Data_Model/distanceCommuneMonitor.rsd")
radius_km <- 20
df_dist <- df_dist %>% filter(dist<radius_km*1e3); rm(radius_km)

df_dist$dist %>% range()
df_dist %>% names()

fig_mapaChile_facet_monitor(df_modelo, mp25, df_dist,
                            limites=c(0,50),
                            titulo = "Promedio 2017-2019 \n MP2.5 [ug/m3]")
f_savePlot(last_plot(),
           file_path =sprintf(file_name,"MapaChileMP25Facet_ExpMonitor"),dpi=300)


data_model %>% 
  fig_mapa(mp25, facets={{facets}}, limites = limites, lwd=0.01,titulo=titulo)+
  geom_sf(data=monitor %>% filter(zona_facet=="Norte"), 
          aes(geometry=geometry), shape=4)+
  labs(title="Norte",subtitle = "Arica a Coquimbo")+
  theme(legend.position = "none")+
  coord_sf(datum = NA,expand=F)

  # Escala colores: https://colorbrewer2.org/
ggplot(data_model) +
  geom_sf(aes(fill = mp25, geometry = geometry), lwd=0.01) +
  # facet_grid({{facets}})+
  scale_fill_distiller(
    palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1,
    name = f_replaceVar("mp25"),
    limits = c(0,50),
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  geom_sf(data=df_dist,aes(geometry=geometry), shape=4)+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 14)

# CDP 
data_model$mrAdj_CDP %>% range(na.rm=T)
ggplot(data_model) +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry), lwd=0.01) +
  # facet_grid({{facets}})+
  scale_fill_distiller(
    palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1,
    name = f_replaceVar("mrAdj_CDP"),
    limits = c(0,1000),
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  coord_trans(fill="sqrt")+
  theme_minimal(base_size = 14)



# EoF