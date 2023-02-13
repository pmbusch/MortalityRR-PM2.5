### MortalityRR-PM2.5
## Cross sectional ecological study - Model construction
## PBH Feb 2023

## Load Data ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
load('.RData')
# file_name <- "Figuras/Analisis_Transversal/Modelos_Mortalidad_All/%s.png"
# file_mod <- "Data/Data_Modelo/Modelos_AllCauses/%s.rsd"
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/05-FunctionsCrossSectional.R", encoding = "UTF-8")

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)
library(ggfortify)

df <- data_model %>% arrange(population) %>% 
  mutate(deathsAdj=deathsAdj_ExtCauses-deathsAdj_SUI,
         Latitude=-map_dbl(geometry, ~st_centroid(.x)[[2]]))

# different endpoints to loop
endpoints <- c("deathsAdj_AllCauses", "deathsAdj_CDP","deathsAdj_CVD",
               "deathsAdj_RSP", "deathsAdj_CAN","deathsAdj_LCA")

# for Table 2 in the article
df_modelConstruction <- tibble(model=as.character(),
                               endpoint=as.character(),
                               n_obs=as.numeric(),
                               aic=as.numeric(),
                               bic=as.numeric(),
                               coef=as.numeric(),
                               low=as.numeric(),
                               high=as.numeric(),
                               ci=as.character())


# MAIN LOOP ----
for (e in endpoints){
 
  # Mod 1 - PM2.5 ONLY ------ 
  
  df <- df %>% rename(endpoint=!!e) # change variable as endpoint
  
  mod1 <- glm.nb(endpoint ~ mp25_10um +offset(log(population)), 
                   data = df,na.action=na.omit)
  
  # store data
  CI <- confint(mod1, method="Wald", level=0.95)["mp25_10um",]
  df_modelConstruction <- rbind(df_modelConstruction,tibble(
    model="PM2.5 only",
    endpoint=e,  
    n_obs=nobs(mod1),
    aic=AIC(mod1),
    bic=BIC(mod1),
    coef=exp(summary(mod1)$coefficients["mp25_10um","Estimate"]) %>% round(2),
    low=exp(CI[1]) %>% round(2),
    high=exp(CI[2]) %>% round(2),
    ci=paste("(",format(low,digits=3),
                ", ",format(high,digits=3),")",sep = ""),
    p_value=summary(mod1)$coefficients["mp25_10um",4]
  ))
  rm(CI,mod1)
  
  # Mod 2 - PM2.5 + Meteorological  ------ 
  mod2 <- glm.nb(endpoint ~ mp25_10um +
                      scale(hr_anual) +
                      scale(heating_degree_15_winter) +
                      offset(log(population)), 
                    data = df,na.action=na.omit)
  
  # store data
  CI <- confint(mod2, method="Wald", level=0.95)["mp25_10um",]
  df_modelConstruction <- rbind(df_modelConstruction,tibble(
    model="PM2.5 + Meteorological",
    endpoint=e,
    n_obs=nobs(mod2),
    aic=AIC(mod2),
    bic=BIC(mod2),
    coef=exp(summary(mod2)$coefficients["mp25_10um","Estimate"]) %>% round(2),
    low=exp(CI[1]) %>% round(2),
    high=exp(CI[2]) %>% round(2),
    ci=paste("(",format(low,digits=3),
             ", ",format(high,digits=3),")",sep = ""),
    p_value=summary(mod2)$coefficients["mp25_10um",4]
  ))
  rm(CI,mod2)
  
  # Mod3 - PM2.5 + Demographic ------ 
  mod3 <- glm.nb(endpoint ~ mp25_10um +
                      scale(urbanDensity) +
                      scale(perc_female) +
                      scale(perc_ethnicityOrig) +
                      scale(perc_rural) +
                      scale(perc_overcrowding_medium)+
                      offset(log(population)), 
                    data = df,na.action=na.omit)
  
  # store data
  CI <- confint(mod3, method="Wald", level=0.95)["mp25_10um",]
  df_modelConstruction <- rbind(df_modelConstruction,tibble(
    model="PM2.5 + Demographic",
    endpoint=e,
    n_obs=nobs(mod3),
    aic=AIC(mod3),
    bic=BIC(mod3),
    coef=exp(summary(mod3)$coefficients["mp25_10um","Estimate"]) %>% round(2),
    low=exp(CI[1]) %>% round(2),
    high=exp(CI[2]) %>% round(2),
    ci=paste("(",format(low,digits=3),
             ", ",format(high,digits=3),")",sep = ""),
    p_value=summary(mod3)$coefficients["mp25_10um",4]
  ))
  rm(CI,mod3)
  
  # Mod4 - PM2.5 + Socioeconomic ------ 
  mod4 <- glm.nb(endpoint ~ mp25_10um +
                      scale(perc_woodHeating) +
                      scale(log(income_median)) + scale(perc_less_highschool) +
                      scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                      offset(log(population)), 
                    data = df,na.action=na.omit)
  
  # store data
  CI <- confint(mod4, method="Wald", level=0.95)["mp25_10um",]
  df_modelConstruction <- rbind(df_modelConstruction,tibble(
    model="PM2.5 + Socioeconomic",
    endpoint=e,
    n_obs=nobs(mod4),
    aic=AIC(mod4),
    bic=BIC(mod4),
    coef=exp(summary(mod4)$coefficients["mp25_10um","Estimate"]) %>% round(2),
    low=exp(CI[1]) %>% round(2),
    high=exp(CI[2]) %>% round(2),
    ci=paste("(",format(low,digits=3),
             ", ",format(high,digits=3),")",sep = ""),
    p_value=summary(mod4)$coefficients["mp25_10um",4]
  ))
  rm(CI,mod4)
  
  
  # Mod Full - Full Model PM2.5 ------ 
  modFull <- glm.nb(endpoint ~ mp25_10um +
                   scale(urbanDensity) +
                   scale(perc_female) +
                   scale(perc_ethnicityOrig) +
                   scale(perc_rural) +
                   scale(perc_woodHeating) +
                   scale(log(income_median)) + scale(perc_less_highschool) +
                   scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                   scale(perc_overcrowding_medium)+
                   scale(hr_anual) +
                   scale(heating_degree_15_winter) +
                   offset(log(population)), 
                 data = df,na.action=na.omit)
  
  # store data
  CI <- confint(modFull, method="Wald", level=0.95)["mp25_10um",]
  df_modelConstruction <- rbind(df_modelConstruction,tibble(
    model="Full Model",
    endpoint=e,
    n_obs=nobs(modFull),
    aic=AIC(modFull),
    bic=BIC(modFull),
    coef=exp(summary(modFull)$coefficients["mp25_10um","Estimate"]) %>% round(2),
    low=exp(CI[1]) %>% round(2),
    high=exp(CI[2]) %>% round(2),
    ci=paste("(",format(low,digits=3),
             ", ",format(high,digits=3),")",sep = ""),
    p_value=summary(modFull)$coefficients["mp25_10um",4]
  ))
  rm(CI,modFull)
  
  # remove column name
  df$endpoint <- NULL
}

df_modelConstruction

# Summary Table -----

# n_ obs
df_modelConstruction %>% group_by(model) %>% summarise(n_obs=mean(n_obs))


data_table <- df_modelConstruction %>% 
  mutate(ci=paste0(coef," ",ci)) %>% 
  dplyr::select(model,endpoint,ci) %>% 
  pivot_wider(names_from = endpoint, values_from = ci)

data_table %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  flextable::align(j=1, align = "left", part="all")
