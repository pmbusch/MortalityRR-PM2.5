### Analisis-COVID-MP2.5
## Analisis Transversal Mortalidad todas las Causas
## PBH Septiembre 2020

## Librerias ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_Transversal/Modelos_Mortalidad_All/%s.png"
file_mod <- "Data/Data_Modelo/Modelos_AllCauses/%s.rsd"
source("Scripts/00-Funciones.R", encoding = "UTF-8")
source("Scripts/05-FuncionesAnalisisTransversal.R", encoding = "UTF-8")

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)

## Variables adicionales ---------
df_modelo %>% names() %>% sort()
df <- df_modelo %>% 
  mutate(mp25_10um=mp25/10, # para ver aumento en RR por 10ug/m3
         mp10_minus25=mp10-mp25)


df_modelo %>% 
  mutate(tasa_mortalidad=def_cardioPulmonar/poblacion*1e5) %>% 
  summarise(mean_tasa=mean(tasa_mortalidad,na.rm=T),
            var_tasa=var(tasa_mortalidad,na.rm=T),
            sd_tasa=sd(tasa_mortalidad, na.rm = T))








## Modelo Base. Y= Causas Cardiopulmonares -------------
# Notar que es poisson
mod_nb <- glm(def_cardioPulmonar ~ mp25_10um +
                mp10_minus25+
                scale(densidad_pob_censal) +
                scale(`15-44`) + scale(`65+`) +
                scale(perc_mujer) +
                scale(perc_puebloOrig) +
                scale(perc_rural) +
                scale(tasa_camas) +
                scale(perc_lenaCalefaccion) +
                scale(log(ingresoAutonomo_media)) + scale(perc_menor_media) +
                scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                scale(perc_vivHacMedio)+
                scale(hr_anual) +
                scale(heating_degree_15_winter) +
                offset(log(poblacion)), 
              data = df,
              family = poisson(link=log),
              na.action=na.omit)

summary(mod_nb)
nobs(mod_nb)
plot(mod_nb)
gam::plot.Gam(mod_nb,se=T,rug=T)
f_tableMRR(mod_nb, preview = "none", highlight = T)
f_figMRR(mod_nb)
f_savePlot(last_plot(), sprintf(file_name,"CardioPulmonar_Base"),dpi=150)
saveRDS(mod_nb, sprintf(file_mod,"CardioPulmonar_Base"))
rm(mod_nb)



## Modelo Base Sign. Y= Causas CardioPulmonares -------------
mod_nb_sign <- glm(def_cardioPulmonar ~ 
                     mp25_10um +
                     scale(`65+`) +
                     scale(log(ingresoAutonomo_media)) + 
                     scale(perc_menor_media) +
                     scale(perc_vivHacMedio)+
                     scale(heating_degree_15_winter) +
                     offset(log(poblacion)), 
                   data = df,
                   family=poisson(link=log),
                   na.action=na.omit)

summary(mod_nb_sign)
nobs(mod_nb_sign)
anova(mod_nb_sign)
f_tableMRR(mod_nb_sign, preview = "none")
f_figMRR(mod_nb_sign)
f_savePlot(last_plot(), sprintf(file_name,"CardioPulmonar"),dpi=150)
saveRDS(mod_nb_sign, sprintf(file_mod,"CardioPulmonar"))
rm(mod_nb_sign)

## Modelo Base Sign. Y= Causas CardioPulmonares 65+ -------------
mod_nb_sign_65 <- glm(def_cardioPulmonar_65 ~ 
                        mp25_10um +
                        scale(log(ingresoAutonomo_media)) + 
                        scale(perc_menor_media) +
                        scale(perc_vivHacMedio)+
                        scale(heating_degree_15_winter) +
                        offset(log(poblacion*`65+`)), 
                      data = df,
                      family=poisson(link=log),
                      na.action=na.omit)

summary(mod_nb_sign_65)
nobs(mod_nb_sign_65)
anova(mod_nb_sign_65)
f_tableMRR(mod_nb_sign_65, preview = "none")
f_figMRR(mod_nb_sign_65)
f_savePlot(last_plot(), sprintf(file_name,"CardioPulmonar_Mayor65"),dpi=150)
saveRDS(mod_nb_sign_65, sprintf(file_mod,"CardioPulmonar_Mayor65"))
rm(mod_nb_sign_65)


## Modelo Base Sign. Y= Causas CardioPulmonares. Quartile PM2.5 -------------
df_cuartil <- df_modelo %>% 
  filter(!is.na(mp25)) %>% 
  mutate(cuartil_mp25=qgroup(mp25, 4))
df_cuartil$cuartil_mp25 %>% table()

label_cuartil <- df_cuartil %>% group_by(cuartil_mp25) %>% 
  summarise(count=n(), mean_mp25=mean(mp25,na.rm=T),
            min_mp=min(mp25,na.rm=T) %>% round(1),
            max_mp=max(mp25,na.rm=T) %>% round(1)) %>% 
  ungroup() %>% 
  mutate(cuartil_label=paste(cuartil_mp25,"\n [",min_mp," - ",max_mp,"]",sep=""))
df_cuartil <- df_cuartil %>% left_join(label_cuartil, by=c("cuartil_mp25"))
df_cuartil$cuartil_label %>% table()

mod_nb_sign_q <- glm(def_cardioPulmonar ~ 
                       cuartil_label +
                       scale(`15-44`) + scale(`65+`) +
                       scale(perc_rural) +
                       scale(perc_lenaCalefaccion) +
                       scale(log(ingresoAutonomo_media)) + 
                       scale(perc_menor_media) +
                       scale(perc_vivHacMedio)+
                       scale(tmed_anual) +
                       scale(heating_degree_15_winter) +
                       offset(log(poblacion)), 
                     data = df_cuartil,
                     family=poisson(link=log),
                     na.action=na.omit)

summary(mod_nb_sign_q)
nobs(mod_nb_sign_q)
anova(mod_nb_sign_q)
f_tableMRR(mod_nb_sign_q, preview = "none")
f_figMRR(mod_nb_sign_q)
rm(mod_nb_sign_q)


## Modelo Base ---------
formula_base <- formula(def_cardioPulmonar ~ 
                          mp25_10um +
                          scale(`65+`) +
                          scale(log(ingresoAutonomo_media)) + 
                          scale(perc_menor_media) +
                          scale(perc_vivHacMedio)+
                          scale(heating_degree_15_winter) +
                          offset(log(poblacion)))
                         
reformulate(deparse(formula_base[[3]]), response = "def_cardio")

## MODELOS CON DISTINTA CAUSA DE DEFUNCION --------------
# Total deaths ----------
mod_total <- glm(reformulate(deparse(formula_base[[3]]), response = "def_total"), 
           data = df,
           na.action=na.omit,
           family=poisson(link=log))

summary(mod_total)
nobs(mod_total)
f_tableMRR(mod_total, preview = "none")
f_figMRR(mod_total)
f_savePlot(last_plot(), sprintf(file_name,"Total"),dpi=150)
saveRDS(mod_total, sprintf(file_mod,"Total"))
rm(mod_total)

# All Cause deaths (no external) ----------
mod_allCauses <- glm(reformulate(deparse(formula_base[[3]]), response = "def_allCauses"), 
                 data = df,
                 na.action=na.omit,
                 family=poisson(link=log))

summary(mod_allCauses)
nobs(mod_allCauses)
f_tableMRR(mod_allCauses, preview = "none")
f_figMRR(mod_allCauses)
f_savePlot(last_plot(), sprintf(file_name,"AllCauses"),dpi=150)
saveRDS(mod_allCauses, sprintf(file_mod,"AllCauses"))
rm(mod_allCauses)


# External Cause deaths ----------
mod_extCauses <- glm(reformulate(deparse(formula_base[[3]]), response = "def_extCauses"), 
                    data = df,
                    na.action=na.omit,
                    family=poisson(link=log))

summary(mod_extCauses)
nobs(mod_extCauses)
f_tableMRR(mod_extCauses, preview = "none")
f_figMRR(mod_extCauses)
f_savePlot(last_plot(), sprintf(file_name,"ExtCauses"),dpi=150)
saveRDS(mod_extCauses, sprintf(file_mod,"ExtCauses"))
rm(mod_extCauses)

# Cardio ----------
mod_cardio <- glm(reformulate(deparse(formula_base[[3]]), response = "def_cardio"), 
                     data = df,
                     na.action=na.omit,
                     family=poisson(link=log))

summary(mod_cardio)
nobs(mod_cardio)
f_tableMRR(mod_cardio, preview = "none")
f_figMRR(mod_cardio)
f_savePlot(last_plot(), sprintf(file_name,"Cardio"),dpi=150)
saveRDS(mod_cardio, sprintf(file_mod,"Cardio"))
rm(mod_cardio)

# Pulmonar ----------
mod_pulmonar <- glm(reformulate(deparse(formula_base[[3]]), response = "def_pulmonar"), 
                     data = df,
                     na.action=na.omit,
                     family=poisson(link=log))

summary(mod_pulmonar)
nobs(mod_pulmonar)
f_tableMRR(mod_pulmonar, preview = "none")
f_figMRR(mod_pulmonar)
f_savePlot(last_plot(), sprintf(file_name,"Pulmonar"),dpi=150)
saveRDS(mod_pulmonar, sprintf(file_mod,"Pulmonar"))
rm(mod_pulmonar)

# Cancer ----------
mod_cancer <- glm(reformulate(deparse(formula_base[[3]]), response = "def_cancer"), 
                     data = df,
                     na.action=na.omit,
                     family=poisson(link=log))

summary(mod_cancer)
nobs(mod_cancer)
f_tableMRR(mod_cancer, preview = "none")
f_figMRR(mod_cancer)
f_savePlot(last_plot(), sprintf(file_name,"Cancer"),dpi=150)
saveRDS(mod_cancer, sprintf(file_mod,"Cancer"))
rm(mod_cancer)



## Loop 65+ -------------
causas <- c("def_total_65", "def_allCauses_65", "def_extCauses_65", "def_cardio_65",
            "def_pulmonar_65", "def_cancer_65","def_cardioPulmonar_65")

formula_base <- formula(def_cardioPulmonar_65 ~ 
                          mp25_10um +
                          scale(log(ingresoAutonomo_media)) + 
                          scale(perc_menor_media) +
                          scale(perc_vivHacMedio)+
                          scale(heating_degree_15_winter) +
                          offset(log(poblacion*`65+`)))

for (c in causas){
  file_path <- paste(str_remove_all(c,"def_|_65"),"_mayor65",sep="")
  cat("Causa: ",c," File: ",file_path,"\n",sep="")
  mod <- glm(reformulate(deparse(formula_base[[3]]), response = c), 
                    data = df,
                    na.action=na.omit,
                    family=poisson(link=log))
  f_savePlot(last_plot(), sprintf(file_name,file_path),dpi=150)
  saveRDS(mod, sprintf(file_mod,file_path))
  rm(mod,file_path)
}
rm(c)

## Loop 75+ -------------
causas <- c("def_total_75", "def_allCauses_75", "def_extCauses_75", "def_cardio_75",
            "def_pulmonar_75", "def_cancer_75","def_cardioPulmonar_75")

formula_base <- formula(def_cardioPulmonar_75 ~ 
                          mp25_10um +
                          scale(log(ingresoAutonomo_media)) + 
                          scale(perc_menor_media) +
                          scale(perc_vivHacMedio)+
                          scale(heating_degree_15_winter) +
                          offset(log(poblacion*`75+`)))

for (c in causas){
  file_path <- paste(str_remove_all(c,"def_|_75"),"_mayor75",sep="")
  cat("Causa: ",c," File: ",file_path,"\n",sep="")
  mod <- glm(reformulate(deparse(formula_base[[3]]), response = c), 
             data = df,
             na.action=na.omit,
             family=poisson(link=log))
  f_savePlot(last_plot(), sprintf(file_name,file_path),dpi=150)
  saveRDS(mod, sprintf(file_mod,file_path))
  rm(mod,file_path)
}
rm(c)

## Loop 30+ -------------
causas <- c("def_total_30", "def_allCauses_30", "def_extCauses_30", "def_cardio_30",
            "def_pulmonar_30", "def_cancer_30","def_cardioPulmonar_30")

formula_base <- formula(def_cardioPulmonar_30 ~ 
                          mp25_10um +
                          scale(log(ingresoAutonomo_media)) + 
                          scale(perc_menor_media) +
                          scale(perc_vivHacMedio)+
                          scale(heating_degree_15_winter) +
                          offset(log(poblacion*`30+`)))

for (c in causas){
  file_path <- paste(str_remove_all(c,"def_|_30"),"_mayor30",sep="")
  cat("Causa: ",c," File: ",file_path,"\n",sep="")
  mod <- glm(reformulate(deparse(formula_base[[3]]), response = c), 
             data = df,
             na.action=na.omit,
             family=poisson(link=log))
  f_savePlot(last_plot(), sprintf(file_name,file_path),dpi=150)
  saveRDS(mod, sprintf(file_mod,file_path))
  rm(mod,file_path)
}
rm(c)


## Modelo Step sobre Y= Causas Cardiopulmonares ------------
# https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
library(caret)

## Creo df solo con variables numericas de interes (y fuera las COVID)
df_modelo %>% names() %>% sort()
df <-  df_modelo %>% 
  mutate(mp10_minus25=mp10-mp25) %>% 
  dplyr::select(
    poblacion,
    def_cardioPulmonar,
    `15-44`,`45-64`,`65-74`,`65+`,`75+`,
    cons_lena_kg,  
    densidad_pob,densidad_pob_censal,
    densidad_pob_manzana_media, densidad_pob_manzana_mediana,
    densidad_pob_manzana_p90, 
    hdd15_winter_lenaCalefaccion, heating_degree_15_anual,
    heating_degree_15_fall,   heating_degree_15_spring,
    heating_degree_15_summer, heating_degree_15_winter,
    heating_degree_18_anual,  heating_degree_18_fall,
    heating_degree_18_spring, heating_degree_18_summer,
    heating_degree_18_winter, hr_anual,
    hr_fall, hr_spring, hr_summer, hr_winter,
    tmed_anual,  tmed_fall, tmed_spring, tmed_summer,tmed_winter,
    ingresoAutonomo_media,ingresoAutonomo_mediana,
    ingresoTotal_media,   ingresoTotal_mediana,
    mp25, mp10_minus25,
    perc_FFAA, perc_fonasa_A, perc_fonasa_B, perc_fonasa_C,
    perc_fonasa_D, perc_isapre, perc_salud,
    perc_lenaAgua, perc_lenaCalefaccion, perc_lenaCocina, 
    perc_material_irrecuperable,perc_menor_media,
    perc_mujer, perc_puebloOrig, perc_rural,
    perc_ocupado, perc_vivAntes2002,perc_vivHacCritico,
    perc_vivHacMedio, perc_vivSinHac
  ) %>% 
  rename(e15_44=`15-44`,e45_64=`45-64`,e65_74=`65-74`,
         e65_plus=`65+`,e75_plus=`75+`) %>% 
  na.omit()
df %>% nrow() #Numero observaciones


# Columnas a remover dado que serian redundantes por su correlacion con otras variables
# identify and eliminate collinear variables
cols <- df %>% 
  cor() %>% 
  findCorrelation()
# Columnas fuera
df[,cols] %>% names() %>% sort()
## Keep def_cardiopulmonar and poblacion
cols <- cols[cols!=1 & cols!=2]
# Columnas remanentes
df[,-cols] %>% names() %>% sort()

df <- df[,-cols]


## Prepare model with an offset
## Fuente: https://stackoverflow.com/questions/61104205/how-can-i-train-a-glmnet-model-poisson-family-with-an-offset-term-using-the-ca

# AIC estimates the relative amount of information lost by a given model: 
# the less information a model loses, the higher the quality of that model
dat <- df %>% dplyr::select(-poblacion)
X = model.matrix(def_cardioPulmonar ~ ., data=dat)
Y = dat$def_cardioPulmonar
OFF = log(df$poblacion)

glm_fit <- caret::train(
  x = cbind(X,OFF),
  y = Y,
  method = "glmStepAIC",
  penalty = c(rep(1,ncol(X)),0), ##Penalty zero for the offset term
  preProcess = c("scale"),
  family = "poisson",
  link="log",
  na.action = na.omit
)

glm_fit
summary(glm_fit)
varImp(glm_fit)
f_tableMRR(glm_fit$finalModel, preview="none")
rm(glm_fit)

## Adjust same model
formula_step <- format(glm_fit$finalModel$formula) %>% 
  paste(collapse = "") %>% 
  str_replace(".outcome", "def_cardioPulmonar") %>% 
  str_replace_all("\\+",") + scale(") %>% 
  str_replace("~","~scale(") %>% 
  str_remove_all(" ") %>% 
  str_replace("scale\\(OFF", "offset(log(poblacion))") %>% 
  formula()
formula_step

mod <- glm(formula_step,
           data=df,
           family=poisson(link=log),
           na.action = na.omit)
summary(mod)
nobs(mod)
f_tableMRR(mod, preview = "none")
f_figMRR(mod)

rm(glm_fit, mod)

