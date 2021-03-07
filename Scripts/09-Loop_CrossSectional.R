### MortalityRR-PM2.5
## Cross sectional ecological study: Loop for multiple models
## PBH NOV 2020


## Load Data ------
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load(".RData")
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figures/%s.png"
# file_mod <- "Data/Data_Model/Models_Loop/%s.rsd"
file_mod <- "Data/Data_Model/Models_Loop_nb/%s.rsd" # Negative binomial
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

# Get dependent variables
names_df <- names(df)
dependent <- names_df[str_detect(names_df,"mr")]
rm(names_df)

explanatory <- c("mp25_10um",
                 "scale(urbanDensity)",
                 "scale(perc_ethnicityOrig)",
                 "scale(perc_rural)",
                 "scale(perc_woodHeating)",
                 "scale(log(income_median))",
                 "scale(perc_less_highschool)",
                 "scale(perc_fonasa_AB)", 
                 "scale(perc_fonasa_CD)",
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
  
  # df %>% dplyr::select(mrAdj_AllCauses,deathsAdj_AllCauses, 
  #                      mr_LCA_0_30,deaths_LCA_0_30,
  #                      population, deaths_aux, mr_aux, pop_aux) %>% view()
  
  # Filter out zero deaths:
  df_aux <- df %>% filter(deaths_aux!=0)
  
  # Minimun of 30 obs to run the regression
  if (nrow(df_aux)>30){
    ## Formula
    formula_model <- reformulate(str_replace(dependent[z],"mr","deaths"), 
                                 termlabels = c(explanatory_dep,
                                                "offset(log(pop_aux))"))
    
    # Model Possion
    # mod_loop <- glm(formula_model,
    #                 data = df_aux,
    #                 family = poisson(link=log),
    #                 na.action=na.omit)
    
    # Binomial
    mod_loop <- glm.nb(formula_model,
                    data = df_aux,
                    na.action=na.omit)
    
    
    saveRDS(mod_loop, sprintf(file_mod,dependent[z]))
  }
  rm(formula_model,mod_loop,explanatory_dep,df_aux)
}
rm(z)

# check
model_cdp<- read_rds(sprintf(file_mod,"mrAdj_CDP"));summary(model_cdp);nobs(model_cdp)
f_tableMRR(model_cdp, highlight = T); rm(model_cdp)

## Load into DF all models---------
library(tools)
library(broom) # to get info of fitted models easily

# url <- "Data/Data_Model/Models_Loop/"
url <- "Data/Data_Model/Models_Loop_nb/"
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
         cause=str_extract(model,"AllCauses|CDP|CVD|RSP|CAN|LCA|ExtCauses|SUI"),
         age=str_extract(model,"30plus|65plus|75plus|allAges|30_64|65_74|0_30"),
         sex=str_extract(model,"female|male")) %>% 
  replace_na(list(age="Adj.", sex="All")) %>% 
  mutate(age=age %>% str_replace("plus","+") %>% str_replace("_","-") %>% 
           str_replace("allAges","All Ages"),
         sex=sex %>% str_replace("male","Male") %>% str_replace("feMale","Female"),
         cause=cause %>% str_replace("AllCauses", "All \n Causes") %>% 
           str_replace("ExtCauses", "Ext. \n Causes"))
    
  
# Join
df_coef_params <- df_coef %>% left_join(df_params, by = c("model"))

## Save data in Excel
# file_path <- "Data/Data_Model/model_params.csv"
file_path <- "Data/Data_Model/model_params_nb.csv"
cat('sep=; \n',file = file_path)
write.table(df_params,file_path, sep=';',row.names = F, append = T)

# file_path <- "Data/Data_Model/model_coef.csv"
file_path <- "Data/Data_Model/model_coef_nb.csv"
cat('sep=; \n',file = file_path)
write.table(df_coef_params,file_path, sep=';',row.names = F, append = T)

# rm(file_path,url,file_name, modelos_rsd, df_coef_params, df_coef, df_params)

## Summary Figure-----------
# file_path <- "Data/Data_Model/model_coef.csv"
# df_coef_params <- read.delim(file_path, sep=";",skip = 1)
df_coef_params %>% names()
df_coef_params$cause %>% unique()
df_coef_params$age %>% unique()
df_coef_params$sex %>% unique()

levels_age <- c("All Ages","Adj.","0-30","30-64","65-74","75+","30+","65+")
levels_age <- c("All Ages","Adj.","0-30","30-64","65-74","75+")
# levels_age <- c("All Ages","0-30","30-64","65-74","75+")
levels_sex <- c("All","Male","Female")
levels_causes <- c("All \n Causes","CDP","CVD","RSP","CAN","LCA","Ext. \n Causes")

# Figure summary for all sex ------
df_coef_params %>% 
  rowid_to_column() %>% 
  filter(term=="mp25_10um") %>% 
  mutate(cause=factor(cause,levels_causes),
         age=factor(age,levels_age),
         sex=factor(sex, levels_sex),
         sign=conf.low>1) %>% 
  filter(!is.na(age),!is.na(sex),!is.na(cause)) %>% # remove models not desired
  ggplot(aes(x=fct_rev(age), y=estimate))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_point(aes(col=sign), size=2, alpha=.5)+
  scale_color_manual(values = c("#666666","red"))+guides(col=F)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_grid(cause~sex)+
  coord_flip(ylim = c(0.8,1.2))+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE),
                     breaks = seq(0.8,1.2,0.1))+
  labs(x="Age group",
       y=expression(paste(
         "MRR: Excess risk per an increase in 10 ","\u03BCg/m\u00B3"," PM2.5"),sep=""), 
       caption="MRR (with C.I. 95%) under different endpoints. Red point indicates a significant effect. \n 
  Columns Grid: Different mortality causes. Rows Grid: Stratified by Sex. Colums Inside: By different age adjustment")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))

# f_savePlot(last_plot(), sprintf(file_name,"MMR_summary"))
f_savePlot(last_plot(), sprintf(file_name,"MMR_summary_nb"))


## Figure summary for all causes -------
levels_causes <- c("All \n Causes","CDP","CVD","RSP","CAN","LCA")
df_coef_params %>% 
  rowid_to_column() %>% 
  filter(term=="mp25_10um") %>% 
  mutate(cause=factor(cause,levels_causes),
         age=factor(age,levels_age),
         sex=factor(sex, levels_sex),
         sign=conf.low>1) %>% 
  filter(!is.na(age),sex=="All",!is.na(cause)) %>% # remove models not desired
  ggplot(aes(x=fct_rev(age), y=estimate))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+
  geom_point(aes(col=sign), size=2, alpha=.5)+
  scale_color_manual(values = c("#666666","red"))+guides(col=F)+
  geom_hline(yintercept = 1, linetype = "dashed")+
  facet_wrap(~cause,ncol = 2)+
  coord_flip(ylim = c(0.7,1.4))+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE),
                     breaks = seq(0.7,1.4,0.1))+
  labs(x="Age group",
       y=expression(paste(
         "MRR: Excess risk per an increase in 10 ","\u03BCg/m\u00B3"," PM2.5"),sep=""), 
       caption="MRR (with C.I. 95%) under different endpoints. Red point indicates a significant effect.")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))


# f_savePlot(last_plot(), sprintf(file_name,"MMR_summary_causes"))
f_savePlot(last_plot(), sprintf(file_name,"MMR_summary_causes_nb"))
## EoF