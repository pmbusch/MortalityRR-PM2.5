### MortalityRR-PM2.5
## Cross sectional ecological study: Loop for multiple models
## PBH NOV 2020


## Load Data ------
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load(".RData")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_Transversal/Modelos_Mortalidad_All/%s.png"
file_mod <- "Data/Data_Model/Models_Loop/%s.rsd"
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/05-FunctionsCrossSectional.R", encoding = "UTF-8")


library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)
library(ggfortify)
library(rlang)

## Base Formula---------
df <- data_model %>% mutate(
  pop30=population*age_30plus, 
  pop65=population*age_65plus, 
  pop75=population*age_75plus)


names_df <- names(df)
dependent <- names_df[str_detect(names_df,"mr")]
rm(names_df)

explanatory <- c("mp25_10um",
                 "scale(urbanDensity)",
                 "scale(perc_ethnicityOrig)",
                 "scale(perc_rural)",
                 "scale(rate_hospitalBeds)",
                 "scale(perc_woodHeating)",
                 "scale(log(income_median))",
                 "scale(perc_less_highschool)",
                 "scale(perc_fonasa_A)", "scale(perc_fonasa_D)",
                 "scale(perc_overcrowding_medium)",
                 "scale(hr_anual)",
                 "scale(heating_degree_15_winter)")

sex <- c("scale(perc_female)")
age <- c("scale(age_15_44)", "scale(age_45_64)","scale(age_65plus)")

## Loop total -----------------
# Name model: MR variable name
length(dependent)
for (z in 1:length(dependent)){
  cat("Model #",z,"\n",sep="")
  # Add age to case all ages
  if (str_detect(dependent[z],"allAges")){
    explanatory_dep <- c(explanatory, age)
  }else{
    explanatory_dep <- explanatory
  }
  
  # Add sex to MR without sex
  if (str_detect(dependent[z],"male")){
    explanatory_dep <- explanatory_dep
  }else{
    explanatory_dep <- c(explanatory_dep,sex)
  }
  
  
  # Estimate population based on MR and Deaths
  df <- df %>% 
    mutate(deaths_aux=!!parse_expr(str_replace(dependent[z],"mr","deaths")),
           mr_aux=!!parse_expr(dependent[z]),
           pop_aux=deaths_aux/mr_aux*1e5,
           pop_aux=as.integer(pop_aux))
  # 
  # df %>% dplyr::select(mrAdj_AllCauses,deathsAdj_AllCauses, 
  #                      population, deaths_aux, mr_aux, pop_aux) %>% view()
  
  # Filter out zero deaths:
  df_aux <- df %>% filter(deaths_aux!=0)
  
  ## Formula
  formula_model <- reformulate(str_replace(dependent[z],"mr","deaths"), 
                               termlabels = c(explanatory_dep,
                                              "offset(log(pop_aux))"))
  
  ## Modelo
  mod_loop <- glm(formula_model, 
                  data = df_aux,
                  family = poisson(link=log),
                  na.action=na.omit)
  
  saveRDS(mod_loop, sprintf(file_mod,dependent[z]))
  rm(formula_model,mod_loop,explanatory_dep,df_aux)
}
rm(z)

# check
model_cdp<- read_rds(sprintf(file_mod,"mrAdj_CDP"));summary(model_cdp)
f_tableMRR(model_cdp); rm(model_cdp)

## Load into DF all models---------
library(tools)
library(broom) # to get info of fitted models easily

url <- "Data/Data_Model/Models_Loop/"
(models_rsd <- list.files(url))

# Save Info
df_params <- data.frame()
df_coef <- data.frame()
i <- 1
## Loop to store data
for (m in models_rsd){
  cat(i," Model: ",m,"\n",sep=""); i <- i+1;
  if(file_ext(m)=="rsd"){
    # load model
    model <- read_rds(paste(url, m, sep=""))
    # summary(model)
    
    ## Parameters model
    param_model <- model %>% glance()
    # param_model
    
    ## Coefficients (we are getting the MRR directly, with exponentiate)
    coef_model <- model %>% tidy(exponentiate = T,  conf.int = T)
    # coef_model
    
    # Store data
    df_params <- rbind(df_params, param_model %>% mutate(model=m))
    df_coef <- rbind(df_coef, coef_model %>% mutate(model=m))
    
    rm(model,param_model, coef_model)
  }
}
rm(i,m)

## Save Data ----------
a <- df_coef; b <- df_params;

# Feat data
df_params$model %>% unique()
df_params <- df_params %>% 
  mutate(dependendent=str_remove(model,".rsd"),
         cause=str_extract(model,"AllCauses|CDP|CVD|RSP|CAN"),
         age=str_extract(model,"30plus|65plus|75plus|allAges"),
         sex=str_extract(model,"female|male")) %>% 
  replace_na(list(age="Adj.", sex="All")) %>% 
  mutate(age=age %>% str_replace("plus","+") %>% str_replace("allAges","All Ages"))
    
  
# Join
df_coef_params <- df_coef %>% left_join(df_params, by = c("model"))

## Save data in Excel
file_path <- "Data/Data_Model/model_params.csv"
cat('sep=; \n',file = file_path)
write.table(df_params,file_path, sep=';',row.names = F, append = T)


file_path <- "Data/Data_Model/model_coef.csv"
cat('sep=; \n',file = file_path)
write.table(df_coef_params,file_path, sep=';',row.names = F, append = T)

# rm(file_path,url,file_name, modelos_rsd, df_coef_params, df_coef, df_params)

## Sumary Figure-----------
# df_coef_params <- read.delim("ResumenModelos/Loop/coef.csv", sep=";",skip = 1)
df_coef_params %>% names()


levels_age <- c("Adj.","All Ages","30+","65+","75+")
levels_sex <- c("All","male","female")
levels_causes <- c("AllCauses","CDP","CVD","RSP","CAN")

df_coef_params %>% 
  rowid_to_column() %>% 
  filter(term=="mp25_10um") %>% 
  mutate(cause=factor(cause,levels_causes),
         age=factor(age,levels_age),
         sex=factor(sex, levels_sex),
         sign=conf.low>1) %>% 
  ggplot(aes(x=fct_rev(age), y=estimate))+
  geom_point(aes(col=sign), size=2, alpha=.5)+
  scale_color_manual(values = c("#666666","black"))+guides(col=F)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(cause~sex)+
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",y="MRR", 
       caption="MRR (with C.I. 95%) under different endpoints. \n 
  Columns Grid: Different mortality causes \n
  Rows Grid: Stratified by Sex \n
  Colums Inside: By different age adjustment")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))



## EoF