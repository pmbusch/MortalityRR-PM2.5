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


lwd_maps <- 0.01
limit_mp25 <- c(0,50)

# Figure PM2.5 ------------
p_north_mp25 <- df %>% filter(zona_facet=="North") %>% 
  ggplot() +
  geom_sf(aes(fill = mp25, geometry = geometry), lwd=lwd_maps) +
  geom_sf(data=monitor %>% filter(zona_facet=="North"), 
          aes(geometry=geometry), shape=4)+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    limits = limit_mp25,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "North",x="", y="") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)

p_center_mp25 <- df %>% filter(zona_facet=="Center") %>% 
  ggplot() +
  geom_sf(aes(fill = mp25, geometry = geometry), lwd=lwd_maps) +
  geom_sf(data=monitor %>% filter(zona_facet=="Center"), 
          aes(geometry=geometry), shape=4)+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    limits = limit_mp25,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "Center",x="", y="") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-76, -69.5), ylim = c(-44.5,-32),datum = NA,expand=F)

p_south_mp25 <- df %>% filter(zona_facet=="South") %>% 
  ggplot() +
  geom_sf(aes(fill = mp25, geometry = geometry), lwd=lwd_maps) +
  geom_sf(data=monitor %>% filter(zona_facet=="South"), 
          aes(geometry=geometry), shape=4)+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    limits = limit_mp25,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "South",x="", y="") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -42),datum = NA,expand=F)

p_stgo_mp25 <- df %>% filter(map_rm==1) %>% 
  ggplot() +
  geom_sf(aes(fill = mp25, geometry = geometry), lwd=lwd_maps) +
  geom_sf(data=monitor %>% filter(mapa_rm==1), 
          aes(geometry=geometry), shape=4)+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    name=expression(paste(
      "PM2.5 2017-2019  [","\u03BCg/m\u00B3","]"),sep=""),
    limits = limit_mp25,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "Santiago",x="", y="",
       caption = "PM2.5 monitor site marked with cross") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "right")+
  coord_sf(xlim = c(-71, -70.4), ylim = c(-33.8, -33.3),datum = NA,expand=F)

# Figure PM2.5
p_north_mp25|p_center_mp25|p_south_mp25|p_stgo_mp25


# Figure Mortality rate CDP -------------
limit_mrCDP <- c(0,360)

p_north_mrCDP <- df %>% filter(zona_facet=="North") %>% 
  mutate(hasMonitor=if_else(commune_valid,"Yes","No")) %>% 
  ggplot() +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry, col=hasMonitor), lwd=lwd_maps) +
  scale_color_manual(values = c("Yes"="black","No"="#666666"))+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    limits = limit_mrCDP,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "North",x="", y="") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)

p_center_mrCDP <- df %>% filter(zona_facet=="Center") %>% 
  mutate(hasMonitor=if_else(commune_valid,"Yes","No")) %>% 
  ggplot() +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry, col=hasMonitor), lwd=lwd_maps) +
  scale_color_manual(values = c("Yes"="black","No"="#666666"))+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    limits = limit_mrCDP,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "Center",x="", y="") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-76, -69.5), ylim = c(-44.5,-32),datum = NA,expand=F)

p_south_mrCDP <- df %>% filter(zona_facet=="South") %>% 
  mutate(hasMonitor=if_else(commune_valid,"Yes","No")) %>% 
  ggplot() +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry, col=hasMonitor), lwd=lwd_maps) +
  scale_color_manual(values = c("Yes"="black","No"="#666666"))+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    limits = limit_mrCDP,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "South",x="", y="") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")+
  coord_sf(xlim = c(-76, -66), ylim = c(-56, -42),datum = NA,expand=F)

p_stgo_mrCDP <- df %>% filter(map_rm==1) %>% 
  mutate(hasMonitor=if_else(commune_valid,"Yes","No")) %>% 
  ggplot() +
  geom_sf(aes(fill = mrAdj_CDP, geometry = geometry, col=hasMonitor), lwd=lwd_maps) +
  scale_color_manual(values = c("Yes"="black","No"="#666666"))+
  scale_fill_distiller(
    palette = "RdYlBu", type = 'seq', na.value = "white", direction = -1,
    name="Adj. MR CDP [per 100,000]",
    limits = limit_mrCDP,
    labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(title = "Santiago",x="", y="", 
       caption="Commune with PM2.5 estimation with black border") + 
  theme_minimal(base_size = 14)+
  theme(legend.position = "right")+
  guides(col=F)+
  coord_sf(xlim = c(-71, -70.4), ylim = c(-33.8, -33.3),datum = NA,expand=F)

# Figure
p_north_mrCDP|p_center_mrCDP|p_south_mrCDP|p_stgo_mrCDP

# Final Figure -----------------
(p_north_mp25|p_center_mp25|p_south_mp25|p_stgo_mp25)/
  (p_north_mrCDP|p_center_mrCDP|p_south_mrCDP|p_stgo_mrCDP)

f_savePlot(last_plot(), file_path = "Figures/map.png",dpi=900)



### Chile side-side -----------------
p_mp25 <- df %>%
  fig_map(mp25, limits = c(0,50), lwd=0.01,title=f_replaceVar("mp25"))+
  geom_sf(data=monitor, aes(geometry=geometry), shape=4)+
  labs(title="PM2.5", caption = "PM2.5 monitor site marked with cross")+
  coord_sf(datum = NA,expand=F)

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
  labs(title="Adj. MR CDP", x="", y="", caption="Commune with PM2.5 estimation with black border")
# p_mr


p_mp25|p_mr


# EoF