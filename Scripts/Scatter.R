### MortalityRR-PM2.5
## Scatter 
## PBH March 2020

## Load Data----
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load('.RData')
source("Scripts/00-Functions.R", encoding = "UTF-8")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
# file_name <- "Scripts/Analisis_General/Figuras/%s.png"



# MR VS PM2.5 ---------

## MR Cardiopulmonar vs PM2.5 -----------

data_model$mrAdj_CDP
p1 <- data_model %>% 
  filter(commune_valid) %>% 
  mutate(nombre_comuna=if_else(population>1e5|mp25>30,
                               nombre_comuna,"")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(mp25, mrAdj_CDP, col=zone))+
  geom_point(alpha=.7, aes(size=population))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x=f_replaceVar("mp25"), 
       y=f_replaceVar("mrAdj_CDP"),
       size=f_replaceVar("population"),
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
# f_savePlot(p1, sprintf(file_name, "Muertes_vs_MP25"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)
# f_savePlot(last_plot(), sprintf(file_name, "Muertes_vs_MP25_name"), dpi=150)


## MR Cardiopulmonar vs Population (n) -----------
# Test wheter n has major implication for MR (more variance)

p1 <- data_model %>% 
  filter(commune_valid) %>%
  mutate(nombre_comuna=if_else(population>1e5|mp25>30,
                               nombre_comuna,""),
         PM25=if_else(!is.na(mp25),"With","Without")) %>% #Label solo pob mayor a 100 mil
  ggplot(aes(population, mrAdj_CDP, col=PM25))+
  geom_point(alpha=.7, aes(size=population))+
  # scale_color_viridis_d()+
  scale_size(labels=function(x) format(x,big.mark = " ", digits=0, scientific = F))+
  guides(colour = guide_legend(override.aes = list(size=4)))+
  labs(x=f_replaceVar("population"), 
       y=f_replaceVar("mrAdj_CDP"),
       size=f_replaceVar("population"),
       color="Monitor PM2.5")+
  theme_bw(20)+theme(panel.grid.major = element_blank())
p1
# f_savePlot(p1, sprintf(file_name, "Muertes_vs_MP25"), dpi=300)
p1+geom_text_repel(aes(label=nombre_comuna), alpha=.8)


# PM2.5 FIXED RADIUS ----------

# Create data frame

df <- df_conc_50km %>% 
  rename(mp25_50=mp25) %>% 
  right_join(data_model,by=c("codigo_comuna"))

df <- df_conc_100km %>% 
  rename(mp25_100=mp25) %>% 
  right_join(df,by=c("codigo_comuna"))

# Population per radius
total_pob <- data_model$population %>% sum()

# 105 communes
pob_mp25 <- df %>% filter(commune_valid) %>% pull(population) %>% sum()
cat(round(pob_mp25/total_pob*100,1),
    "% population in communes with PM2.5 exposure estimated")

# 20km
pob_mp25 <- df %>% filter(!is.na(mp25)) %>% pull(population) %>% sum()
cat(round(pob_mp25/total_pob*100,1), "% population in communes with PM2.5 exposure estimated")

# 50km
pob_mp25 <- df %>% filter(!is.na(mp25_50)) %>% pull(population) %>% sum()
cat(round(pob_mp25/total_pob*100,1), "% population in communes with PM2.5 exposure estimated")

# 100km
pob_mp25 <- df %>% filter(!is.na(mp25_100)) %>% pull(population) %>% sum()
cat(round(pob_mp25/total_pob*100,1), "% population in communes with PM2.5 exposure estimated")



# Make facet plot (long format)
df <- df %>% 
  filter(commune_valid) %>% 
  mutate(latitude=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  dplyr::select(mp25,mp25_50,mp25_100,zone,latitude) %>% 
  pivot_longer(c(mp25_50,mp25_100),
               names_to = "radius",values_to="pm25_new") %>% 
  mutate(radius=if_else(radius=="mp25_50",
                        "PM2.5 Fixed radius 50km",
                        "PM2.5 Fixed radius 100km"))

# Scatter
df %>% 
  ggplot(aes(mp25,pm25_new,col=zone))+
  geom_point(alpha=.7)+
  facet_wrap(~radius)+
  labs(x="PM2.5 Fixed radius 20km", 
       y="PM2.5 with higher radius",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())

f_savePlot(last_plot(),
           "Figures/FixedRadius_PM25/Scatter_Radius.png")

# Differences
df %>% 
  mutate(dif_mp25=mp25-pm25_new) %>% 
  arrange(latitude) %>% 
  ggplot(aes(latitude,dif_mp25,col=zone))+
  geom_point(alpha=.7)+
  facet_wrap(~radius)+
  coord_flip()+
  labs(x="Latitude", 
       y="Difference PM2.5 Fixed radius 20km vs X km",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())

f_savePlot(last_plot(),
           "Figures/FixedRadius_PM25/Diff_Radius.png")


# ADJ VS CRUDE MR ------

# Data adjustment first
df <- data_model %>% 
  filter(commune_valid) %>% 
  mutate(latitude=map_dbl(geometry, ~st_centroid(.x)[[2]])) %>% 
  dplyr::select(nombre_comuna,latitude,zone, mr_CDP_allAges,mrAdj_CDP,
                mr_RSP_allAges,mrAdj_RSP,
                mr_CVD_allAges,mrAdj_CVD,
                mr_CAN_allAges,mrAdj_CAN,
                mr_LCA_allAges,mrAdj_LCA,
                mr_AllCauses_allAges,mrAdj_AllCauses,
                mr_ExtCauses_allAges,mrAdj_ExtCauses) %>% 
  pivot_longer(c(-nombre_comuna,-latitude,-zone),
               names_to="key",values_to="value") %>% 
  mutate(cause=str_remove_all(key,"mr_|mrAdj_|_allAges") %>% 
           factor(levels=c("AllCauses","CDP","CVD","RSP",
                           "CAN","LCA","ExtCauses")),
         type_mr=str_extract(key,"mr_|mrAdj") %>% 
           str_replace("mr_","Crude") %>% 
           str_replace("mrAdj","Adjusted"),
         key=NULL) %>% 
  pivot_wider(names_from=type_mr, values_from = value)

# Scatter
df %>% 
  filter(cause!="LCA") %>% 
  ggplot(aes(Crude,Adjusted,col=zone))+
  geom_point(alpha=.7)+
  facet_wrap(~cause, scales="free")+
  # coord_flip()+
  geom_abline(slope=1, intercept=0, linetype="dashed")+
  labs(x="Crude mortality rate [per 100,000]", 
       y="Adjusted mortality rate [per 100,000]",
       color="",
       caption="Dashed line shows the identity line")+
  theme_bw(20)+theme(panel.grid.major = element_blank())


f_savePlot(last_plot(),
           "Figures/Crude_vs_Adj_MR/CrudevsAdj.png")


# Differences
df %>%
  filter(cause!="LCA") %>% 
  mutate(diff_mr=Crude-Adjusted) %>% 
  arrange(latitude) %>% 
  ggplot(aes(latitude,diff_mr,col=zone))+
  geom_point(alpha=.7)+
  facet_wrap(~cause, scales="free")+
  # coord_flip()+
  geom_abline(slope=0, intercept=0, linetype="dashed")+
  labs(x="Latitude", 
       y="Difference Crude vs Adjusted mortality rate [per 100,000]",
       color="")+
  theme_bw(20)+theme(panel.grid.major = element_blank())

## EoF