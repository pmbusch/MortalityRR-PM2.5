### MortalityRR-PM2.5
## Scatter 
## PBH Nov 2020

## Load Data----
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load('.RData')
source("Scripts/00-Functions.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# file_name <- "Scripts/Analisis_General/Figuras/%s.png"

df_modelo %>% group_by(zona) %>% summarise(count=n())
df_modelo %>% group_by(zona,region) %>% summarise(count=n())

df_modelo <- df_modelo %>% 
  mutate(Zona=case_when(
    zona=="Norte" ~ "Norte: Arica a Coquimbo",
    zona=="Centro" ~ "Centro: Valparaiso a Maule",
    zona=="RM"~ "Region Metropolitana",
    zona=="Sur"~"Sur: Biobio a Los Rios",
    zona=="Austral"~"Austral: Los Lagos a Magallanes",
    T ~ "s/i") %>% 
      factor(levels = c("Norte: Arica a Coquimbo","Region Metropolitana",
                        "Centro: Valparaiso a Maule","Sur: Biobio a Los Rios",
                        "Austral: Los Lagos a Magallanes")))


df_modelo <- df_modelo %>% 
  mutate(MR_allCauses=def_allCauses/poblacion*1e5,
         MR_CVD=def_cardio/poblacion*1e5,
         MR_pulmonary=def_pulmonar/poblacion*1e5,
         MR_cardioPulmonary=def_cardioPulmonar/poblacion*1e5,
         MR_cancer=def_cancer/poblacion*1e5)


## MR Cardiopulmonar vs PM2.5 -----------

df_modelo$MR_cardioPulmonary %>% range()
p1 <- df_modelo %>% 
  filter(Zona!="s/i") %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, MR_cardioPulmonary, col=Zona))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x=f_replaceVar("mp25"), 
       y=f_replaceVar("MR_cardioPulmonary"),
       size=f_replaceVar("poblacion"),
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
# f_savePlot(p1, sprintf(file_name, "Muertes_vs_MP25"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
# f_savePlot(last_plot(), sprintf(file_name, "Muertes_vs_MP25_name"), dpi=150)


## MR Cardiopulmonar vs Population (n) -----------
# Test wheter n has major implication for MR (more variance)

p1 <- df_modelo %>% 
  mutate(nombre_comuna=if_else(poblacion>1e5|mp25>30,
                               nombre_comuna,""),
         PM25=if_else(!is.na(mp25),"With","Without")) %>% #Label solo pob mayor a 100 mil
  # filter(!is.na(mp25)) %>% 
  ggplot(aes(poblacion, MR_cardioPulmonary, col=PM25))+
  geom_point(alpha=.7, aes(size=poblacion))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x=f_replaceVar("poblacion"), 
       y=f_replaceVar("MR_cardioPulmonary"),
       size=f_replaceVar("poblacion"),
       color="Monitor PM2.5")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
# f_savePlot(p1, sprintf(file_name, "Muertes_vs_MP25"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)

## EoF