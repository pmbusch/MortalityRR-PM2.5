### MortalityRR-PM2.5
## PDF summarizing commune data used in the analysis
## PBH Feb. 2020

## Carga Datos a nivel de comuna-----
load(".RData")
source("Scripts/00-Functions.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Paper_escritura/Supplementary/CommuneData/%s.pdf"

# Librerias especiales
library(cowplot)
library(grid)
library(rlang)
library(officer)

# Data for the the pdf ------
df_pdf <- map_commune %>% 
  mutate(centroid=st_centroid(geometry),
         longitude=map_dbl(geometry, ~st_centroid(.x)[[1]]),
         latitude=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  select(codigo_comuna,longitude, latitude) %>% 
  right_join(data_model, by=c("codigo_comuna")) %>% 
  filter(!is.na(latitude)) %>% arrange(desc(latitude))

# Filter only data for the 105 communes
df_pdf <- df_pdf %>% 
  filter(commune_valid)

df_pdf$geometry.x <- NULL
df_pdf$geometry.y <- NULL


## Function to generate all summary figures ---------
f_summaryFigures <- function(df, var){
  # Summary table
  tabla <- df %>% select((!!sym(var))) %>% 
    summarise(Mean=mean((!!sym(var)),na.rm=T),
              SD=sd((!!sym(var)),na.rm=T),
              Min=min((!!sym(var)),na.rm=T),
              P25=quantile((!!sym(var)),0.25,na.rm=T),
              Median=median((!!sym(var)),na.rm=T),
              P75=quantile((!!sym(var)),0.75,na.rm=T),
              Max=max((!!sym(var)),na.rm=T),
              `Obs`=sum(!is.na((!!sym(var)))),
              `Missing obs`=sum(is.na((!!sym(var)))),
              `% Completeness`=(1-`Missing obs`/n())*100)
  tabla <- tabla %>% mutate_all(function(x) round(x,2))
  names_tabla <- names(tabla)
  tabla <- rbind(names_tabla, tabla) %>% t() %>% as.data.frame()
  rm(names_tabla)
  
  ## Flextable as plot object
  tabla <- tabla %>% flextable() %>% 
    autofit(add_w = 0.1, add_h = 0.2) %>%
    align(j=1, align = "left") %>% 
    delete_part(part = "header") %>% 
    flextable::border(j=1:2,i=1, part="body",
                      border.top = fp_border(style = "solid", width=2))
  
  tabla_fig <- ggplot()+theme_void()+
    annotation_custom(rasterGrob(as_raster(tabla)), 
                      xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
  rm(tabla)
  
  # Graph  jitter
  comuna_label <- df_pdf %>%
    mutate(comuna_label=case_when(
      # nombre_comuna=="General Lagos" ~ "Norte",
      # nombre_comuna=="Cabo de Hornos" ~ "Sur",
      nombre_comuna %in% c("Calama","Santiago",
                           "Talca","Concepcion","Temuco",
                           "Coihaique") ~ nombre_comuna,
      T ~ "")) %>% pull(comuna_label)
  comuna_label <- rev(comuna_label)  
  
  p_jitter <- df_pdf %>% 
    ggplot(aes(x=reorder(nombre_comuna,latitude),y=(!!sym(var))))+
    geom_point(alpha=.3)+
    scale_y_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    scale_x_discrete(labels=comuna_label)+
    coord_flip(expand = T)+
    labs(x="",y="")+
    theme(axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.title = element_text(hjust = 0.5, size=14))+
    ggtitle("Dispersion for each commune: North to South")
  
  # Density plot
  p_dens <- ggplot(df, aes((!!sym(var))))+
    geom_density(fill="brown", alpha=.5)+
    scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    coord_cartesian(expand = F)+
    labs(y="",x="")+
    theme(plot.title = element_text(hjust = 0.5, size=14))+
    ggtitle("Density (distribution)")
  
  # Grafico ECDF
  p_ecdf <- ggplot(df, aes((!!sym(var))))+
    stat_ecdf(col="black", size=1)+
    scale_x_continuous(labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
    coord_cartesian(expand = F)+
    labs(y="",x="")+
    theme(plot.title = element_text(hjust = 0.5, size=14))+
    ggtitle("ECDF")
  
  ## Gather plots
  p <- plot_grid(tabla_fig, p_jitter, p_dens, p_ecdf, ncol=2)
  
  # Title
  title <- ggdraw() +
    draw_label(paste(f_addTypeVar(var), f_replaceVar(var),sep=": "), fontface='bold')
  p <- plot_grid(title,p, ncol=1, rel_heights=c(0.1, 1))
  return(p)
}

# f_summaryFigures(df_pdf, "perc_isapre")
# f_summaryFigures(df_pdf, "perc_woodHeating")


## Iteration for columns of interests: Explanatory variables --------------
options(warn=-1) # supress warnings
col_names <- c("mp25",
               "population","urbanDensity",
               "age_15_44", "age_45_64", "age_65plus",
               "perc_female", 
               "perc_rural", "perc_ethnicityOrig",
               "perc_overcrowding_medium","perc_overcrowding_high",
               "income_median_usd","perc_less_highschool", "perc_occupancy",
               "perc_isapre", "perc_fonasa_CD", "perc_fonasa_AB", 
               "perc_woodCooking","perc_woodHeating","perc_woodWarmWater",
               "hr_summer", "hr_winter", "tmed_summer", "tmed_winter",
               "heating_degree_15_summer", "heating_degree_15_winter")



# Sort by variable type
col_names <- tibble(type=f_addTypeVar(col_names),
                    nombre=col_names) %>% arrange(type) %>% pull(nombre)


pdf(sprintf(file_name, "Summary_ExplanatoryVariables"), 
    width = 14.87, height = 9.30)
for (c in col_names){
  f_summaryFigures(df_pdf, c) %>% print()
}
dev.off()
options(warn=0) # activate warnings

## Iteration for columns of interests: Dependent variables --------------
options(warn=-1) # supress warnings
col_names <- df_pdf %>% names()
cols_interest <- col_names %>% str_detect("mr_*|mrAdj*")
col_names <- col_names[cols_interest]

# Sort by variable type
col_names <- tibble(type=f_addTypeVar(col_names),
                    nombre=col_names) %>% arrange(type) %>% pull(nombre)


pdf(sprintf(file_name, "Summary_DependentVariables"), 
    width = 14.87, height = 9.30)
for (c in col_names){
  f_summaryFigures(df_pdf, c) %>% print()
}
dev.off()
options(warn=0) # activate warnings


## EoF