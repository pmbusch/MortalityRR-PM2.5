### MortalityRR-PM2.5
## Cross sectional ecological study
## PBH NOV 2020

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

# Mean vs Var: Overdispersion
df %>% filter(commune_valid) %>% 
  pull(deathsAdj_CDP) %>% mean()
df %>% filter(commune_valid) %>% 
  pull(deathsAdj_CDP) %>% var()


# Base Model. Y= CDP -------------
# Poisson distribution
mod_poisson <- glm(deathsAdj_CDP ~ mp25_10um +
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
                   data = df,
                   family = poisson(link=log),
                   # weights = log(population),
                   na.action=na.omit)

summary(mod_poisson)
confint(mod_poisson, method="Wald")
nobs(mod_poisson)
f_tableMRR(mod_poisson, preview = "none", highlight = T)
f_figMRR(mod_poisson)
autoplot(mod_poisson) # Residuals and regression fit plot
gam::plot.Gam(mod_poisson,se=T,rug=T,terms = "mp25")


# f_savePlot(last_plot(), sprintf(file_name,"CardioPulmonar_Base"),dpi=150)
# saveRDS(mod_poisson, sprintf(file_mod,"CardioPulmonar_Base"))
# rm(mod_poisson)

# heteroscedasticity test
library(lmtest)
# Breusch-Pagan test
bptest(mod_poisson)
# Goldfeld-Quandt test: n* equals to 3/8 of the data used
# Sorted by population
gqtest(mod_poisson, fraction= 2/8*nobs(mod_poisson), point= 0.5,
       alternative = "greater")

# Without outliers in QQ plot and residuals vs fitted ------
mod_poisson_filter <- glm(deathsAdj_CDP ~ mp25_10um +
                     # mp10_minus25+
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
                   data = df[c(-294,-296,-346,-344,-320),],
                   family = poisson(link=log),
                   # weights = log(population),
                   na.action=na.omit)

summary(mod_poisson_filter)
nobs(mod_poisson_filter)
f_tableMRR(mod_poisson_filter, preview = "none", highlight = T)
autoplot(mod_poisson_filter) # Residuals and regression fit plot

## Points 344 (Santiago) and 320 (Calama) seems to have major influence on the model
df[c(320,344),] %>% pull(nombre_comuna)


## Model Binomial -------------
mod_nb <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                   # scale(Latitude) +
                   offset(log(population)), 
                 data = df,
                 na.action=na.omit)


summary(mod_nb)
nobs(mod_nb)
f_tableMRR(mod_nb, preview = "none", highlight = T)
f_figMRR(mod_nb)
autoplot(mod_nb)
gam::plot.Gam(mod_nb,se=T,rug=T,terms = "mp25")

# Residuals plot map ----
df_map <- df %>% 
  filter(commune_valid) %>% 
  mutate(deathsAdj_CDP_Predicted=predict(mod_nb,type="response"),
         mrAdj_CDP=deathsAdj_CDP/population*1e5,
         mrAdj_CDP_predicted=deathsAdj_CDP_Predicted/population*1e5,
         residual_mrAdjCDP=mrAdj_CDP-mrAdj_CDP_predicted)

df_map <- left_join(map_commune, df_map, by=c("codigo_comuna")) 


# Note: need to run functions on InterctiveMaps.R script
leaflet(df_map) %>% 
  addTiles() %>% 
  f_leafleft(df_map,df_map$residual_mrAdjCDP,
             "residual_mrAdjCDP", 
             unidad = "deaths per 100,000 habs") %>% 
  addLayersControl(
    baseGroups = c("OpenStreetMap","Toner", "Toner by Stamen"),
    overlayGroups = c("residual_mrAdjCDP")) %>% 
  hideGroup(c("residual_mrAdjCDP"))
rm(df_map)

# Moran index ------------ 
# for spatial autocorrelation, for the residuals
# see https://mgimond.github.io/Spatial/spatial-autocorrelation-in-r.html
library(spdep)
# remove NA's
df_aux <- df_map %>% dplyr::select(residual_mrAdjCDP) %>% na.omit()
dim(df_aux)
# remove neighbors with no links
df_aux <- df_aux[-c(1,49,56,57),]
# neighbors between coomunes
(nb <- poly2nb(df_aux, queen=T))
lw <- nb2listw(nb, style="W", zero.policy=TRUE)
# Moran
moran.test(df_aux$residual_mrAdjCDP, lw)
# simulation to compute p-value
MC <- moran.mc(df_aux$residual_mrAdjCDP, lw, nsim=599)
MC
plot(MC, main="", las=1)

# Binomial model with bootstrap -----
# Source: https://stackoverflow.com/questions/54749641/bootstrapping-with-glm-model

# data structure for results
nboot <- 1000
bres <- matrix(NA,
               nrow=nboot,
               ncol=length(coef(mod_nb)),
               dimnames=list(rep=seq(nboot),
                             coef=names(coef(mod_nb))))
# bootstrap
set.seed(101)
bootsize <- 105
df_boot <- df %>% filter(commune_valid)
for (i in seq(nboot)) {
  bdat <- df_boot[sample(nrow(df_boot),size=bootsize,replace=TRUE),]
  bfit <- update(mod_nb, data=bdat)  ## refit with new data
  bres[i,] <- coef(bfit)
}
# output
data.frame(mean_est=colMeans(exp(bres)),
           t(apply(exp(bres),2,quantile,c(0.025,0.975))))

# Spatial Lagged Model --------------
df_neighbor <- df %>% filter(commune_valid) %>% .[-c(1,49,56,57),]
# local mean using neighbors
df_neighbor$loc_mean <- lapply(lw$neighbours, 
                              function(i) mean(df_neighbor$mrAdj_CDP[i])) %>% 
  unlist()
# correlation
cor(df_neighbor$mrAdj_CDP,df_neighbor$loc_mean)
# model
mod_nb_spatialLag <- glm.nb(deathsAdj_CDP ~ mp25_10um +
                              scale(loc_mean)+
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
                            data = df_neighbor,
                            na.action=na.omit)

summary(mod_nb_spatialLag)
nobs(mod_nb_spatialLag)
f_tableMRR(mod_nb_spatialLag, preview = "none", highlight = T)



# Comparison nb vs poisson
anova(mod_poisson, mod_nb)
# Likelihood ratio test
lrtest(mod_poisson,mod_nb)
# Negative binomial is a better fit

# heteroscedasticity test
# Breusch-Pagan test
bptest(mod_nb)
gqtest(mod_nb, fraction= 2/8*nobs(mod_nb), point= 0.5,
       alternative = "greater")



## Base Model only Sign -------------
mod_nb_sign <-glm.nb(deathsAdj_CDP ~ mp25_10um +
                            scale(urbanDensity) +
                            # scale(perc_female) +
                            # scale(perc_ethnicityOrig) +
                            # scale(perc_rural) +
                            scale(perc_woodHeating) +
                            scale(log(income_median)) + scale(perc_less_highschool) +
                            scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                            scale(perc_overcrowding_medium)+
                            scale(hr_anual) +
                            # scale(heating_degree_15_winter) +
                            offset(log(population)), 
                          data = df,
                          na.action=na.omit)

# summary(mod_nb_sign)
nobs(mod_nb_sign)
f_tableMRR(mod_nb_sign, preview = "none", highlight = T)
f_figMRR(mod_nb_sign)


## Base Model  Quartile PM2.5 -------------
df_quartile <- data_model %>% 
  filter(!is.na(mp25_10um) & commune_valid) %>% 
  mutate(quartile_pm25=qgroup(mp25_10um, 4))
df_quartile$quartile_pm25 %>% table()

label_cuartil <- df_quartile %>% group_by(quartile_pm25) %>% 
  summarise(count=n(), mean_mp25=mean(mp25_10um,na.rm=T),
            min_mp=min(mp25_10um,na.rm=T) %>% round(1),
            max_mp=max(mp25_10um,na.rm=T) %>% round(1)) %>% 
  ungroup() %>% 
  mutate(cuartil_label=paste(quartile_pm25,"\n [",min_mp," - ",max_mp,"]",sep=""))
df_quartile <- df_quartile %>% left_join(label_cuartil, by=c("quartile_pm25"))
df_quartile$cuartil_label %>% table()

mod_nb_quartile <- glm.nb(deathsAdj_CDP ~ quartile_pm25 +
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
                          data = df_quartile,
                          na.action=na.omit)

summary(mod_nb_quartile)
nobs(mod_nb_quartile)
f_tableMRR(mod_nb_quartile, preview = "none", highlight = T)
# f_figMRR(mod_nb_quartile)
# rm(mod_nb_quartile)


## Model with dummy if has monitor ------------------
data_aux <- df %>% 
  mutate(hasMonitor=!is.na(mp25))

mod_nb_dummy <- glm.nb(deathsAdj_CDP ~ hasMonitor +
                         scale(urbanDensity) +
                         scale(perc_female) +
                         scale(perc_ethnicityOrig) +
                         scale(perc_rural) +
                         scale(perc_woodHeating) +
                         scale(log(income_median)) + scale(perc_less_highschool) +
                         scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                         scale(perc_overcrowding_medium)+
                         # scale(hr_anual) +
                         # scale(heating_degree_15_winter) +
                         offset(log(population)), 
                       data = data_aux,
                       na.action=na.omit)

summary(mod_nb_dummy)
nobs(mod_nb_dummy)
f_tableMRR(mod_nb_dummy, preview = "none", highlight = T)
# f_figMRR(mod_nb_dummy)
# rm(mod_nb_dummy)


## Interaction with wood ---------------
mod_nb_interaction <- glm.nb(deathsAdj_CDP ~ mp25_10um*scale(perc_woodHeating) +
                                 scale(urbanDensity) +
                                 scale(perc_female) +
                                 scale(perc_ethnicityOrig) +
                                 scale(perc_rural) +
                                 # scale(perc_woodHeating) +
                                 scale(log(income_median)) + scale(perc_less_highschool) +
                                 scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                                 scale(perc_overcrowding_medium)+
                                 scale(hr_anual) +
                                 scale(heating_degree_15_winter) +
                                 offset(log(population)), 
                               data = df,
                               na.action=na.omit)

summary(mod_nb_interaction)
nobs(mod_nb_interaction)
f_tableMRR(mod_nb_interaction, preview = "none", highlight = T)
f_figMRR(mod_nb_interaction)
autoplot(mod_nb_interaction) # Residuals and regression fit plot
gam::plot.Gam(mod_nb_interaction,se=T,rug=T)
# Marginal effect
# library(margins)
# margins(mod_poisson_interaction)
# cplot(mod_poisson_interaction, "mp25_10um")


## Population above 50K -------------
df %>% filter(!is.na(mp25)&population>50*1e3) %>% nrow()
mod_nb_pop <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                     data = df %>% filter(population>50*1e3),
                     # weights = log(population),
                     na.action=na.omit)

summary(mod_nb_pop)
nobs(mod_nb_pop)
f_tableMRR(mod_nb_pop, preview = "none", highlight = T)
# f_figMRR(mod_nb_pop)

## Without 10% smallest in population communes -------------
data_pop <- df %>% 
  arrange(population) %>% 
  filter(commune_valid==1)
# remove 10%
# 105 
data_pop <- data_pop[-1:-11,]

mod_pop_high <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                       data = data_pop,
                       # weights = log(population),
                       na.action=na.omit)

# summary(mod_pop_high)
nobs(mod_pop_high)
f_tableMRR(mod_pop_high, preview = "none", highlight = T)
# f_figMRR(mod_pop_high)

## Without 10% less and high polluted communes -------------
data_aux <- df %>% 
  arrange(mp25) %>% 
  filter(commune_valid==1)
# remove 10%
# 105 / 10  = 11
data_aux <- data_aux[-95:-105,]
data_aux <- data_aux[-1:-11,]

mod_high_pm25 <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                        data = data_aux,
                        # weights = log(population),
                        na.action=na.omit)

# summary(mod_high_pm25)
nobs(mod_high_pm25)
f_tableMRR(mod_high_pm25, preview = "none", highlight = T)
# f_figMRR(mod_pop_high)


# Without rm  -------
mod_nb_rmOut <- glm.nb(deathsAdj_CDP ~ mp25_10um +
                         # mp10_minus25+
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
                       data = data_model %>% filter(region!="Metropolitana"),
                       na.action=na.omit)

# summary(mod_nb_rmOut)
nobs(mod_nb_rmOut)
f_tableMRR(mod_nb_rmOut, preview = "none", highlight = T)
# f_figMRR(mod_nb_rmOut)

# Only RM  -------
mod_nb_rm <- glm.nb(deathsAdj_CDP ~ mp25_10um +
                      # mp10_minus25+
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
                    data = data_model %>% filter(region=="Metropolitana"),
                    na.action=na.omit)

# summary(mod_nb_rmOut)
nobs(mod_nb_rm)
f_tableMRR(mod_nb_rm, preview = "none", highlight = T)
# f_figMRR(mod_nb_rmOut)


## Fixed Radius 50km ------------
df <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  left_join(df_conc_50km, by=c("codigo_comuna")) %>% 
  mutate(mp25_10um=mp25/10)

mod_rad50km <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                         data = df,
                         na.action=na.omit)

# summary(mod_rad50km)
nobs(mod_rad50km)
f_tableMRR(mod_rad50km, preview = "none", highlight = T)


## Fixed Radius 100km ------------
df <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  left_join(df_conc_100km, by=c("codigo_comuna")) %>% 
  mutate(mp25_10um=mp25/10)

mod_rad100km <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                       data = df,
                       na.action=na.omit)

# summary(mod_rad100km)
nobs(mod_rad100km)
f_tableMRR(mod_rad100km, preview = "none", highlight = T)

## Fixed Radius 200km ------------
df <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  left_join(df_conc_200km, by=c("codigo_comuna")) %>% 
  mutate(mp25_10um=mp25/10)

mod_rad200km <- glm.nb(deathsAdj_CDP ~ mp25_10um +
                         scale(urbanDensity) +
                         scale(perc_female) +
                         scale(perc_ethnicityOrig) +
                         scale(perc_rural) +
                         scale(perc_woodHeating) +
                         scale(log(income_median)) + scale(perc_less_highschool) +
                         scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                         scale(perc_overcrowding_medium)+
                         # scale(hr_anual) +
                         # scale(heating_degree_15_winter) +
                         offset(log(population)), 
                       data = df,
                       na.action=na.omit)
nobs(mod_rad200km)
f_tableMRR(mod_rad200km, preview = "none", highlight = T)

## Fixed Radius Infinity ------------
df <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  left_join(df_conc_Infkm, by=c("codigo_comuna")) %>% 
  mutate(mp25_10um=mp25/10)

mod_radInf <- glm.nb(deathsAdj_CDP ~ mp25_10um +
                       scale(urbanDensity) +
                       scale(perc_female) +
                       scale(perc_ethnicityOrig) +
                       scale(perc_rural) +
                       scale(perc_woodHeating) +
                       scale(log(income_median)) + scale(perc_less_highschool) +
                       scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                       scale(perc_overcrowding_medium)+
                       # scale(hr_anual) +
                       # scale(heating_degree_15_winter) +
                       offset(log(population)), 
                     data = df,
                     na.action=na.omit)
nobs(mod_radInf)
f_tableMRR(mod_radInf, preview = "none", highlight = T)

## Interaction age vs NSE -------------
mod_nb_intNSE <- glm.nb(deaths_CDP_allAges ~ mp25_10um +
                          scale(urbanDensity) +
                          scale(age_15_44)+
                          scale(age_45_64)+
                          scale(age_65plus)+
                          scale(perc_female) +
                          scale(perc_ethnicityOrig) +
                          scale(perc_rural) +
                          scale(perc_woodHeating) +
                          scale(log(income_median)) + scale(perc_less_highschool) +
                          scale(perc_fonasa_AB) + scale(perc_fonasa_CD) +
                          scale(perc_overcrowding_medium)+
                          scale(hr_anual) +
                          scale(heating_degree_15_winter) +
                          scale(log(income_median))* scale(age_65plus)+
                          offset(log(population)), 
                        data = df,
                        na.action=na.omit)

# summary(mod_nb_intNSE)
nobs(mod_nb_intNSE)
f_tableMRR(mod_nb_intNSE, preview = "none", highlight = T)
gam::plot.Gam(mod_nb_intNSE,se=T,rug=T,terms = "mp25")

# Without high NSE coomunes ------
# See which communes to remove
# Las Condes, Vitacura, Providencia
df %>% 
  filter(commune_valid) %>%
  filter(income_median_usd>500) %>% 
  ggplot(aes(reorder(nombre_comuna, income_median_usd),
             income_median_usd))+
  geom_col()+
  coord_flip()+
  xlab("Comuna")+ylab("Median monthly income USD")+
  theme_bw(20)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
  

mod_nb_NSE <- glm.nb(deathsAdj_CDP ~ mp25_10um +
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
                        data = df %>% 
                       filter(!(nombre_comuna %in% c("Las Condes", "Vitacura", "Providencia"))),
                        na.action=na.omit)

nobs(mod_nb_NSE)
f_tableMRR(mod_nb_NSE, preview = "none", highlight = T)


# Log linear regression -------
mod_lm <- lm(log(deathsAdj_CDP) ~ mp25_10um +
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
             data =df,
             na.action=na.omit)
summary(mod_lm)
# R2: 0.99


##Step Model  Y = CDP ------------
# https://stats.stackexchange.com/questions/20836/algorithms-for-automatic-model-selection/20856#20856
library(caret)

## DF with only numerical variables of interest
data_model %>% names() %>% sort()
df <-  data_model %>% 
  dplyr::select(
    population,
    deathsAdj_CDP,
    urbanDensity_mean,urbanDensity_median,urbanDensity_mean_p90,
    urbanDensity,
    heating_degree_15_anual,
    heating_degree_15_fall,   heating_degree_15_spring,
    heating_degree_15_summer, heating_degree_15_winter,
    heating_degree_18_anual,  heating_degree_18_fall,
    heating_degree_18_spring, heating_degree_18_summer,
    heating_degree_18_winter, hr_anual,
    hr_fall, hr_spring, hr_summer, hr_winter,
    tmed_anual,  tmed_fall, tmed_spring, tmed_summer,tmed_winter,
    income_mean,income_median,
    mp25_10um, 
    # mp10_minus25,
    perc_woodCooking,perc_woodHeating,perc_woodWarmWater,perc_wood_avg,
    perc_FFAA, perc_fonasa_AB, perc_fonasa_CD, perc_isapre, 
    # perc_health,
    perc_less_highschool,perc_occupancy,
    perc_female,perc_ethnicityOrig,perc_rural,
    perc_overcrowding_low,perc_overcrowding_medium,perc_overcrowding_high) %>% 
  na.omit()
df %>% nrow() # Number of obs

# cor(df$mp25_10um, df$perc_health) # Almost no correlation

# We remove colums with redundant information, based on their correlation with other variables
# identify and eliminate collinear variables
cols <- df %>% 
  cor() %>% 
  findCorrelation()
# Columnas fuera
df[,cols] %>% names() %>% sort()
## Keep dependent and population
cols <- cols[cols!=1 & cols!=2]
# Columnas remanentes
df[,-cols] %>% names() %>% sort()
df <- df[,-cols]

## Prepare model with an offset
## Fuente: https://stackoverflow.com/questions/61104205/how-can-i-train-a-glmnet-model-poisson-family-with-an-offset-term-using-the-ca

# AIC estimates the relative amount of information lost by a given model: 
# the less information a model loses, the higher the quality of that model
dat <- df %>% dplyr::select(-population)
X = model.matrix(deathsAdj_CDP ~ ., data=dat)
Y = dat$deathsAdj_CDP
OFF = log(df$population)

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

f_tableMRR(glm_fit$finalModel, preview="none", highlight = T)
# rm(glm_fit)

## Adjust same model
formula_step <- format(glm_fit$finalModel$formula) %>% 
  paste(collapse = "") %>% 
  str_replace(".outcome", "deathsAdj_CDP") %>% 
  str_replace_all("\\+",") + scale(") %>% 
  str_replace("~","~scale(") %>% 
  str_remove_all(" ") %>% 
  str_replace("scale\\(OFF", "offset(log(population))") %>% 
  formula()
formula_step

mod <- glm(formula_step,
           data=df,
           family=poisson(link=log),
           na.action = na.omit)
summary(mod)
nobs(mod)
f_tableMRR(mod, preview = "none", highlight=T)
f_figMRR(mod)

rm(glm_fit, mod)

## EoF