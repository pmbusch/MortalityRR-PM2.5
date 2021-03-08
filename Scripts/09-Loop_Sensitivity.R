### MortalityRR-PM2.5
## Loop to study sensitivy analysis
## PBH March 2021

## Load Data ------
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load(".RData")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figures/Sensitivity/%s.png"
file_mod <- "Data/Data_Model/Sensitivity_Loop/%s/%s.rsd"
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/05-FunctionsCrossSectional.R", encoding = "UTF-8")


## Base parameters---------
df <- data_model
df_50km <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  left_join(df_conc_50km, by=c("codigo_comuna")) %>% 
  mutate(mp25_10um=mp25/10)
df_100km <- data_model %>% 
  dplyr::select(codigo_comuna, nombre_comuna,
                deathsAdj_AllCauses, deathsAdj_CDP,deathsAdj_CVD,
                deathsAdj_RSP, deathsAdj_CAN,deathsAdj_LCA,deathsAdj_ExtCauses,
                urbanDensity, perc_female,perc_ethnicityOrig,perc_rural,
                perc_woodHeating,income_median, perc_less_highschool,
                perc_fonasa_AB,perc_fonasa_CD,perc_overcrowding_medium,
                hr_anual, heating_degree_15_winter, population) %>% 
  left_join(df_conc_100km, by=c("codigo_comuna")) %>% 
  mutate(mp25_10um=mp25/10)
df_extremePM25 <- df %>% 
  arrange(mp25) %>% 
  filter(commune_valid==1)
# remove 10%
# 105 / 10  = 11
df_extremePM25 <- df_extremePM25[-95:-105,]
df_extremePM25 <- df_extremePM25[-1:-11,]


# Get dependent variables
# names_df <- names(df)
# dependent <- names_df[str_detect(names_df,"mr")]
# rm(names_df)
dependent <- c("deathsAdj_AllCauses","deathsAdj_CDP","deathsAdj_CVD",
              "deathsAdj_RSP","deathsAdj_CAN","deathsAdj_LCA")

# Loops to generate models ---------
for (z in 1:length(dependent)){
  cat("Model #",z,"\n",sep="")
  
  # Generate auxiliar dependent variables
  df <- df %>% 
    mutate(deaths_aux=!!parse_expr(dependent[z]))
  # Filter out zero deaths:
  df_aux <- df %>% filter(deaths_aux!=0)
  
  df_50km_aux <- df_50km %>% 
    mutate(deaths_aux=!!parse_expr(dependent[z])) %>% 
    filter(deaths_aux!=0)
  
  df_100km_aux <- df_100km %>% 
    mutate(deaths_aux=!!parse_expr(dependent[z])) %>% 
    filter(deaths_aux!=0)
  
  df_extremePM25_aux <- df_extremePM25 %>% 
    mutate(deaths_aux=!!parse_expr(dependent[z])) %>% 
    filter(deaths_aux!=0)
  
  # Minimun of 30 obs to run the regression
  if (nrow(df_aux)>30){
    # Base
    mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                     data = df_aux,
                     na.action=na.omit)
    saveRDS(mod_loop, sprintf(file_mod,"Base",dependent[z]))
    
    # Population above 50K
    mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                         data = df_aux %>% filter(population>50*1e3),
                         na.action=na.omit)
    saveRDS(mod_loop, sprintf(file_mod,"Population commune  50K",dependent[z]))
  
    # Only rm
    mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                           data = df_aux %>% filter(region=="Metropolitana"),
                           na.action=na.omit)
    saveRDS(mod_loop, sprintf(file_mod,"Only Metropolitan region",dependent[z]))
    
    # without rm
    mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                       data = df_aux %>% filter(region!="Metropolitana"),
                       na.action=na.omit)
    saveRDS(mod_loop, sprintf(file_mod,"Without Metropolitan region",dependent[z]))
  }
  
  # 50 km
  mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                     data = df_50km_aux,
                     na.action=na.omit)
  saveRDS(mod_loop, sprintf(file_mod,"Fixed Radius 50km",dependent[z]))
  
  # 100 km
  mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                     data = df_100km_aux,
                     na.action=na.omit)
  saveRDS(mod_loop, sprintf(file_mod,"Fixed Radius 100km",dependent[z]))
  
  # Extreme PM2.5
  mod_loop <- glm.nb(deaths_aux ~ mp25_10um +
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
                     data = df_extremePM25_aux,
                     na.action=na.omit)
  saveRDS(mod_loop, sprintf(file_mod,"10% in each extreme of PM2.5 excluded",dependent[z]))

  rm(mod_loop,df_aux,df_extremePM25_aux, df_100km_aux, df_50km_aux)
}
rm(z)

# check
model_cdp<- read_rds(sprintf(file_mod,"Base","deathsAdj_CDP"));summary(model_cdp);nobs(model_cdp)
f_tableMRR(model_cdp, highlight = T); rm(model_cdp)


## Load into DF all models---------
library(tools)
library(broom) # to get info of fitted models easily

url <- "Data/Data_Model/Sensitivity_Loop/"
(folders_model <- list.files(url))

# Save Info
df_params <- data.frame()
df_coef <- data.frame()
i <- 1

# Loop inside each folder
for (f in folders_model){
  url_aux <- paste0(url,f,"/")
  models_rsd <- list.files(url_aux)
  for (m in models_rsd){
    cat(i," Model: ",f,", ",m,"\n",sep=""); i <- i+1;
    if(file_ext(m)=="rsd"){
      # load model
      model <- read_rds(paste(url_aux, m, sep=""))
      # summary(model)
      
      ## Parameters model
      param_model <- model %>% glance()
      # param_model
      
      ## Coefficients (we are getting the MRR directly, with exponentiate)
      coef_model <- model %>% tidy(exponentiate = T,  conf.int = T)
      # coef_model
      
      # Store data
      df_params <- rbind(df_params, param_model %>% mutate(case=f,
                                                           model=m))
      df_coef <- rbind(df_coef, coef_model %>% mutate(case=f,
                                                      model=m))
      
      rm(model,param_model, coef_model)
    }
  }
}
rm(i,m,f)

## Save Data ----------
a <- df_coef; b <- df_params;

# Feat data
df_params$model %>% unique()
df_params$case %>% unique()
df_params <- df_params %>% 
  mutate(dependent=str_remove(model,".rsd"),
         cause=str_extract(model,"AllCauses|CDP|CVD|RSP|CAN|LCA|ExtCauses|SUI")) %>% 
  mutate(cause=cause %>% str_replace("AllCauses", "All \n Causes") %>% 
           str_replace("ExtCauses", "Ext. \n Causes"))

# Join
df_coef_params <- df_coef %>% 
  left_join(df_params, by = c("model","case")) %>% 
  mutate(case=case %>% str_replace_all("Population commune  50K",
                                       "Population commune >50,000"))
  
## Summary figure ----------------
df_coef_params$cause %>% unique()
levels_causes <- c("All \n Causes","CDP","CVD","RSP","CAN","LCA")
levels_causes <- c("All \n Causes","CDP","CVD","RSP")
levels_case <- c("Base", "Fixed Radius 50km", "Fixed Radius 100km",
                "Population commune >50,000", 
                "Only Metropolitan region","Without Metropolitan region",
                "10% in each extreme of PM2.5 excluded")


# Figure summary  ------
df_coef_params %>% 
  rowid_to_column() %>% 
  filter(term=="mp25_10um") %>% 
  mutate(cause=factor(cause,levels_causes),
         case=factor(case,levels_case),
         sign=conf.low>1) %>% 
  filter(!is.na(cause)) %>% # remove models not desired
  ggplot(aes(x=fct_rev(case), y=estimate))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_point(aes(col=sign), size=2, alpha=.5)+
  scale_color_manual(values = c("#666666","red"))+guides(col=F)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~cause)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",
       y=expression(paste(
         "MRR: Excess risk per an increase in 10 ","\u03BCg/m\u00B3"," PM2.5"),sep=""), 
       caption="MRR (with C.I. 95%) under different endpoints. Red point indicates a significant effect.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))

# f_savePlot(last_plot(), sprintf(file_name,"MMR_summary"))
f_savePlot(last_plot(), sprintf(file_name,"MMR_summary_nb"))

