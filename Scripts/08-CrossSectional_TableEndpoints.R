### MortalityRR-PM2.5
## Cross sectional ecological study: Summary Table  with different Endpoints
## PBH NOV 2020

## Load Data ------
theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figuras/Analisis_Transversal/Modelos_Mortalidad_All/%s.png"
file_mod <- "Data/Data_Modelo/Modelos_AllCauses/%s.rsd"
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/05-FunctionsCrossSectional.R", encoding = "UTF-8")

library(MASS)
library(lme4)
library(glmmTMB)
library(gamm4)
library(ggfortify)


# Generate deaths as count number
df <- data_model %>% 
  mutate(deaths_CDP=mrAdj_CDP*population/1e5,
         deaths_CDP=as.integer(deaths_CDP),
         deaths_AllCauses=mrAdj_AllCauses*population/1e5,
         deaths_AllCauses=as.integer(deaths_AllCauses),
         deaths_CVD=mrAdj_CVD*population/1e5,
         deaths_CVD=as.integer(deaths_CVD),
         deaths_RSP=mrAdj_RSP*population/1e5,
         deaths_RSP=as.integer(deaths_RSP),
         deaths_CAN=mrAdj_CAN*population/1e5,
         deaths_CAN=as.integer(deaths_CAN))



# Base Model Formula ----------------
formula_initial <- formula(deaths_CDP ~ 
                             mp25_10um +
                             scale(urbanDensity) +
                             scale(perc_female) +
                             scale(perc_ethnicityOrig) +
                             scale(perc_rural) +
                             scale(rate_hospitalBeds) +
                             scale(perc_woodHeating) +
                             scale(log(income_median)) + scale(perc_less_highschool) +
                             scale(perc_fonasa_A) + scale(perc_fonasa_D) +
                             scale(perc_overcrowding_medium)+
                             scale(hr_anual) +
                             scale(heating_degree_15_winter) +
                             offset(log(population)))


## Run models with different endpoint ------------
endpoints <- c("deaths_CDP", "deaths_AllCauses","deaths_CVD",
               "deaths_RSP", "deaths_CAN")

# mod_CDP <- glm(reformulate(deparse(formula_initial[[3]]), response = "deaths_CDP"), 
#                data = df, na.action=na.omit, family=poisson(link=log))


## Loop to extract info from all models
i <- 0
for (e in endpoints){
  # Run Model
  mod <- glm(reformulate(deparse(formula_initial[[3]]), response = e), 
                 data = df, na.action=na.omit, family=poisson(link=log))
  
  
  
  # Get Coefficients
  est <- summary(mod)$coefficients[,1:4] %>% as.data.frame() %>% 
    as_tibble(rownames = "parametro")
  names(est) <- c("parametro","coef","sd","z_value","p_value")
  
  ## Add codes
  est <- est %>% mutate(codes=case_when(
    p_value<0.001 ~ "***",
    p_value<0.01 ~ "**",
    p_value<0.05 ~ "*",
    p_value<0.1 ~ ".",
    T ~ ""))
  
  ## Format: RR (p-value) stars
  est <- est %>% 
    mutate(coef=exp(coef) %>% round(2),
           p_value=round(p_value,2),
           table_display=paste(format(coef,digits=2),
                               " (",format(p_value, digits=2),") ",
                               codes,sep=""))
  # Select only columns of interest
  est <- est %>% dplyr::select(parametro, table_display)
  
  # Names
  names(est) <- c("parametro", all.vars(mod$formula)[1])
  
  ## Add additional parameters
  dev <- summary(mod)$deviance %>% round(2) %>% format(digits=2)
  aic <- summary(mod)$aic %>% round(2) %>% format(digits=2)
  
  est <- rbind(est,
               c("n",nobs(mod)),
               c("Residual Deviance",dev),
               c("AIC", aic))
  
  # Join
  if (i!=0){
    est_all <- est_all %>% left_join(est, by = c("parametro"))
  } else{
    est_all <- est
  }
  
  i <- i+1
}
# remove intercept
est_all <- est_all[-1,]


## Summary Table --------------

foot_note <- c("MRR (p-value)",
               "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")

names(est_all) <- c("Variable","Adj. MR CDP","Adj. MR All Causes","Adj. MR CVD",
                    "Adj. MR RSP","Adj. MR CAN")

est_all %>% 
  mutate(Variable=Variable %>% 
           str_remove_all("scale|\\(|\\)|log") %>% 
           f_replaceVar()) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  bold(j=1, bold=T) %>% 
  flextable::border(part="body",i=14,
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=1:2, value=as_paragraph(foot_note), part="header", inline=T) %>% 
  print(preview="docx")


# EoF