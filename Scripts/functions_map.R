### MortalityRR-PM2.5
## Functions to generate Maps
## PBH Nov 2020

theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# load(".RData")
# source("Scripts/00-Functions.R", encoding = "UTF-8")
library(patchwork)

## Maps ## ---------
# Heat map for Chile
# Uses as base the variable map_commune
# One can filter the df before to zoom an specific geographic zone
# Inputs: df, value to heatmap, facet, scale, title, filename, lwd
fig_map <- function(df, val, facets=NULL, limits=NULL, 
                     title="", fileName=NULL, lwd=0.5){
  p <- ggplot(df) +
    geom_sf(aes(fill = {{val}}, geometry = geometry), lwd=lwd) +
    facet_grid({{facets}})+
    # Escala colores: https://colorbrewer2.org/
    scale_fill_distiller(
      palette = "YlOrRd", type = 'seq', na.value = "white", direction = 1,
      name = title,
      limits = limits,
      labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    labs(title = "",x="", y="") + coord_sf(datum = NA, expand = FALSE)+
    theme_minimal(base_size = 14)
  # Guardo
  if (!is.null(fileName)){
    f_savePlot(p,fileName)
  }
  p
}



## Figura con zoom a partes de Chile de mayor interes
## Figure with Chile divided in 4 zones: North, Center, South, Patagonia
## Uses fig_map function
fig_mapChile_facet <- function(df, val, facets=NULL, limits=NULL, 
                                title="", fileName=NULL){
  
  # Zones to facet, based on regions
  df <- df %>% mutate(zona_facet=case_when(
    codigo_region %in% c("15","01","02","03","04") ~ "North",
    codigo_region %in% c("05","13","06","07") ~ "Center",
    codigo_region %in% c("08","16","09","14") ~ "South",
    codigo_region %in% c("10","11","12") ~ "Patagonia") %>% factor())
  
  # Creo los graficos, con coordenadas fijas para cada zona (mejora la estetica)
  p1 <- df %>% filter(zona_facet=="North") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    labs(title="North",subtitle = "Arica-Coquimbo")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)
  p2 <- df %>% filter(zona_facet=="Center") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    labs(title="Center",subtitle = "RM-V-Talca")+
    coord_sf(xlim = c(-73, -69.5), ylim = c(-37, -32),datum = NA, expand=F)
  p3 <- df %>% filter(zona_facet=="South") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    labs(title="South",subtitle = "Biobio-Los Rios")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-74, -70.5), ylim = c(-41, -36),datum = NA,expand=F)
  p4 <- df %>% filter(zona_facet=="Patagonia") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    labs(title="Patagonia",subtitle = "Pto Montt-Punta Arenas")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-76, -66), ylim = c(-56, -40),datum = NA,expand=F)
  
  p_region <- map_region %>% 
    left_join(df %>% select(codigo_region, zona_facet) %>% unique()) %>% 
    ggplot()+
    geom_sf(aes(geometry=geometry, fill=zona_facet),alpha=.3)+
    scale_fill_viridis_d(option = "inferno")+
    coord_sf(xlim=c(-76,-66), ylim=c(-56, -17.5), datum = NA,expand=F)+
    theme_minimal(base_size = 14)+
    theme(legend.position = "none")
  
  # Nota: alineacion de la leyenda fue dificil, solucion heuristica actual es buena
  p <- (p1|p2)/(p3|p4)+plot_layout(guide="auto")&
    theme(plot.subtitle = element_text(size=8))
  
  # best solution so far to include mapa chile completo
  p <- plot_spacer()|p_region|p
  
  # Save
  if (!is.null(fileName)){
    f_savePlot(p,fileName, dpi=300)
  }
  p
}



# Additional feature:  monitor location
fig_mapChile_facet_monitor <- function(df, val, monitor, facets=NULL, limits=NULL, 
                                        title="", fileName=NULL){
  # Zones to facet, based on regions
  df <- df %>% mutate(zona_facet=case_when(
    codigo_region %in% c("15","01","02","03","04") ~ "North",
    codigo_region %in% c("05","13","06","07") ~ "Center",
    codigo_region %in% c("08","16","09","14") ~ "South",
    codigo_region %in% c("10","11","12") ~ "Patagonia") %>% factor())
  
  monitor <- monitor %>% mutate(zona_facet=case_when(
    codigo_region %in% c("15","01","02","03","04") ~ "North",
    codigo_region %in% c("05","13","06","07") ~ "Center",
    codigo_region %in% c("08","16","09","14") ~ "South",
    codigo_region %in% c("10","11","12") ~ "Patagonia") %>% factor())
  
  
  p1 <- df %>% filter(zona_facet=="North") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    geom_sf(data=monitor %>% filter(zona_facet=="North"), 
            aes(geometry=geometry), shape=4)+
    labs(title="North",subtitle = "Arica-Coquimbo")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-72, -66.5), ylim = c(-32.5, -17.5),datum = NA,expand=F)
  p2 <- df %>% filter(zona_facet=="Center") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    geom_sf(data=monitor %>% filter(zona_facet=="Center"), 
            aes(geometry=geometry), shape=4)+
    labs(title="Center",subtitle = "RM-V-Talca")+
    coord_sf(xlim = c(-73, -69.5), ylim = c(-37, -32),datum = NA, expand=F)
  p3 <- df %>% filter(zona_facet=="South") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    geom_sf(data=monitor %>% filter(zona_facet=="South"),
            aes(geometry=geometry), shape=4)+
    labs(title="South",subtitle = "Biobio-Los Rios")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-74, -70.5), ylim = c(-41, -36),datum = NA,expand=F)
  p4 <- df %>% filter(zona_facet=="Patagonia") %>% 
    fig_map({{val}}, facets={{facets}}, limits = limits, lwd=0.01,title=title)+
    geom_sf(data=monitor %>% filter(zona_facet=="Patagonia"),
            aes(geometry=geometry), shape=4)+
    labs(title="Patagonia",subtitle = "Pto Montt-Punta Arenas")+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-76, -66), ylim = c(-56, -40),datum = NA,expand=F)
  
  p_region <- map_region %>% 
    left_join(df %>% select(codigo_region, zona_facet) %>% unique()) %>% 
    ggplot()+
    geom_sf(aes(geometry=geometry, fill=zona_facet),alpha=.3)+
    scale_fill_viridis_d(option = "inferno")+
    coord_sf(xlim=c(-76,-66), ylim=c(-56, -17.5), datum = NA,expand=F)+
    theme_minimal(base_size = 14)+
    theme(legend.position = "none")
  
  # Nota: alineacion de la leyenda fue dificil, solucion heuristica actual es buena
  p <- (p1|p2)/(p3|p4)+plot_layout(guide="auto")&
    theme(plot.subtitle = element_text(size=8))
  
  # best solution so far to include mapa chile completo
  p <- plot_spacer()|p_region|p
  
  # Guardo
  if (!is.null(fileName)){
    f_savePlot(p,fileName, dpi=300)
  }
  p
}



## SCATTER COMUNA -----------
# Funcion para graficar en puntos valores de comuna
# Se ordena segun geografia de las regiones y latitud de las comunas
# Df debe estar con join a codigos territoriales y mapa comuna
# Recibe dataframe, valor a graficar, facets, escala, titulo leyenda, guardar
fig_scatterComuna <- function(df, val, col_aes=NULL, limites=NULL,
                              titulo=""){
  df %>% 
    mutate(cent_lat=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
    ggplot(aes(x=reorder(codigo_comuna, cent_lat), y={{val}}, col={{col_aes}}))+
    geom_point()+
    facet_grid(region~., scales = "free", space="free")+
    labs(x="Comunas",y=titulo)+
    scale_y_continuous(limits = limites,
                       labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    scale_color_viridis_d()+
    coord_flip()+
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major.y = element_blank())
}



## BARRAS REGION-COMUNA --------
# Funcion para graficar en barras algun valor promedio
# Se ordena segun geografia de las regiones, y se muestran las comunas de Chile
# Df debe estar con join a codigos territoriales y mapa regiones (levels region)
# Se recomienda filtrar antes por las comunas con MP, para evitar un grafico gigante
# Recibe dataframe, valor a graficar, facets, escala, titulo leyenda, guardar
fig_barrasRegion <- function(df, val, fill_col=NULL, limites=NULL,
                             titulo=""){
  df %>% 
    ggplot(aes(x=reorder(nombre_comuna, {{val}}), y={{val}}, fill={{fill_col}})) +
    geom_col()+
    # geom_hline(yintercept = val_highlight, col="red", linetype = "dashed", size=1)+
    facet_grid(region~., scales = "free", space="free")+
    coord_flip(clip="off")+
    scale_fill_viridis_d()+
    # scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
    scale_x_discrete(name = NULL)+
    scale_y_continuous(name=titulo,
                       expand = c(0, 0),
                       limits=limites,
                       labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
          panel.grid.major.y = element_blank())
}

# df_prueba %>% mutate(highlight=if_else(valor>20,"yes","no")) %>%
#   fig_barrasRegion(valor, fill_col = highlight)
# 
# fig_barrasRegion(df_prueba, valor)

# df_prueba <- df_avg %>% 
#   group_by(comuna) %>% 
#   summarise(valor=mean(valor, na.rm=T)) %>% ungroup() %>% 
#   mutate(nombre_comuna=f_remover_acentos(comuna) %>% 
#            str_replace_all("Aysen","Aisen") %>% 
#            str_replace_all("Coyhaique","Coihaique")) %>% 
#   left_join(codigos_territoriales,by=c("nombre_comuna")) %>% 
#   left_join(mapa_regiones)



## EoF