### MortalityRR-PM2.5
## Generate map figure for paper
## PBH Nov 2020

## Load -------
load(".RData")
source("Scripts/00-Functions.R", encoding = "UTF-8")
# file_name <- "Scripts/Analisis_Exploratorios/Figuras/Mapas/%s.png"
source("Scripts/functions_map.R", encoding = "UTF-8")


## MP2.5 + Monitor --------
## Load Distance data
df_dist <- read_rds("Data/Data_Model/distanceCommuneMonitor.rsd")
radius_km <- 20
df_dist <- df_dist %>% filter(dist<radius_km*1e3); rm(radius_km)

df_dist$dist %>% range()
df_dist %>% names()


fig_mapChile_facet(data_model, mp25,
                            limits=c(0,50),
                            title = f_replaceVar("mp25"))


fig_mapChile_facet_monitor(data_model, mp25, df_dist,
                           limits=c(0,50),
                           title = f_replaceVar("mp25"))







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
data_model %>% filter(!is.na(mp25)) %>% pull(mrAdj_CDP) %>% range(na.rm=T)
ggplot(data_model) +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry), lwd=0.01) +
  # facet_grid({{facets}})+
  scale_fill_distiller(
    palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1,
    name = f_replaceVar("mrAdj_CDP"),
    limits = c(0,360),
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 14)





## Manually generate a figure -------------------
# PM2.5 vs adj. MR CDP

# 3 Zones: North, Center, South
df <- data_model %>% mutate(zona_facet=case_when(
  codigo_region %in% c("15","01","02","03","04") ~ "North",
  codigo_region %in% c("05","13","06","07",
                       "08","16","09","14","10") ~ "Center",
  codigo_region %in% c("11","12") ~ "South") %>% factor())

monitor <- df_dist %>% mutate(zona_facet=case_when(
  codigo_region %in% c("15","01","02","03","04") ~ "North",
  codigo_region %in% c("05","13","06","07",
                       "08","16","09","14","10") ~ "Center",
  codigo_region %in% c("11","12") ~ "South") %>% factor())

# North
p_north_mp25 <- df %>% filter(zona_facet=="North") %>% 
  fig_map(mp25, limits = c(0,50), lwd=0.01,title=f_replaceVar("mp25"))+
  geom_sf(data=monitor %>% filter(zona_facet=="North"), 
          aes(geometry=geometry), shape=4)+
  labs(title="PM2.5")+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)
# p_north_mp25
p_north_mr <- df %>% filter(zona_facet=="North") %>% 
  fig_map(mrAdj_CDP, limits = c(0,360), lwd=0.01,title="Adj. MR CDP [per 100,000]")+
  theme(legend.position = "none")+
  labs(title="Adj. MR CDP")+
  coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)
# p_north_mr

# Center
p_center_mp25 <- df %>% filter(zona_facet=="Center") %>% 
  fig_map(mp25, limits = c(0,50), lwd=0.01,title=f_replaceVar("mp25"))+
  geom_sf(data=monitor %>% filter(zona_facet=="Center"), 
          aes(geometry=geometry), shape=4)+
  # labs(title="PM2.5")+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-76, -69.5), ylim = c(-44.5,-32),datum = NA,expand=F)
# p_center_mp25
p_center_mr <- df %>% filter(zona_facet=="Center") %>% 
  fig_map(mrAdj_CDP, limits = c(0,360), lwd=0.01,title="Adj. MR CDP [per 100,000]")+
  theme(legend.position = "none")+
  # labs(title="Adj. MR CDP")+
  coord_sf(xlim = c(-76, -69.5), ylim = c(-44.5,-32),datum = NA,expand=F)
# p_center_mr

# South
p_south_mp25 <- df %>% filter(zona_facet=="South") %>% 
  fig_map(mp25, limits = c(0,50), lwd=0.01,title=f_replaceVar("mp25"))+
  geom_sf(data=monitor %>% filter(zona_facet=="South"), 
          aes(geometry=geometry), shape=4)+
  # labs(title="PM2.5")+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -42),datum = NA,expand=F)
# p_south_mp25
p_south_mr <- df %>% filter(zona_facet=="South") %>% 
  fig_map(mrAdj_CDP, limits = c(0,360), lwd=0.01,title="Adj. MR CDP [per 100,000]")+
  theme(legend.position = "none")+
  # labs(title="Adj. MR CDP")+
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -42),datum = NA,expand=F)
# p_south_mr


p <- p_north_mp25|p_north_mr
p

(p_north_mp25|p_north_mr)|(p_center_mp25|p_center_mr)|(p_south_mp25|p_south_mr)

(p_north_mp25|p_north_mr)/(p_center_mp25|p_center_mr)/(p_south_mp25|p_south_mr)+
  plot_layout(guide="auto")


library(cowplot)
plot_grid(p_north_mp25,p_north_mr,p_center_mp25,p_center_mr,p_south_mp25,p_south_mr,
          nrow = 3,)





# Nota: alineacion de la leyenda fue dificil, solucion heuristica actual es buena
p <- (p1|p2)/(p3|p4)+plot_layout(guide="auto")&
  theme(plot.subtitle = element_text(size=8))

# best solution so far to include mapa chile completo
p <- plot_spacer()|p_region|p


### Chile side-side -----------------
p_mp25 <- df %>%
  fig_map(mp25, limits = c(0,50), lwd=0.01,title=f_replaceVar("mp25"))+
  geom_sf(data=monitor, aes(geometry=geometry), shape=4)+
  labs(title="PM2.5", caption = "Monitor site marked with cross")+
  

p_mr <- df %>% 
  mutate(hasMonitor=if_else(is.na(mp25),"No","Yes")) %>% 
  ggplot() +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry, col=hasMonitor),lwd=0.001) +
  scale_color_manual(values = c("Yes"="black","No"="#666666"))+
  scale_fill_distiller(
    palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1,
    name = "Adj. MR CDP [per 100,000]",
    limits = c(0,360),
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  coord_sf(datum = NA, expand = FALSE)+
  theme_minimal(base_size = 14)+
  guides(col=F)+
  labs(title="Adj. MR CDP", x="", y="", caption="Commune with monitor with black border")
p_mr


p_mp25|p_mr


# EoF