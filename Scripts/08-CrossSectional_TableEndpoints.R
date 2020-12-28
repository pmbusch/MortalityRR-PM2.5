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


# Base Model Formula ----------------
formula_initial <- formula(deathsAdj_CDP ~ 
                             mp25_10um +
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
                             offset(log(population)))


## Run models with different endpoint ------------
endpoints <- c("deathsAdj_AllCauses", "deathsAdj_CDP","deathsAdj_CVD",
               "deathsAdj_RSP", "deathsAdj_CAN","deathsAdj_LCA","deathsAdj_ExtCauses")

# mod_CDP <- glm(reformulate(deparse(formula_initial[[3]]), response = "deaths_CDP"), 
#                data = df, na.action=na.omit, family=poisson(link=log))

df <- data_model
## Loop to extract info from all models
i <- 0
for (e in endpoints){
  # cat(e," \n",sep="")
  # Run Model: Poisson
  mod <- glm(reformulate(deparse(formula_initial[[3]]), response = e),
                 data = df, na.action=na.omit, family=poisson(link=log))
  
  # Binomial
  # mod <- glm.nb(reformulate(deparse(formula_initial[[3]]), response = e), 
  #            data = df, na.action=na.omit)
  
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
  names(est) <- c("parametro", e)
  
  
  ## Add additional parameters
  # dev <- summary(mod)$deviance %>% round(2) %>% format(digits=2)
  # aic <- summary(mod)$aic %>% round(2) %>% format(digits=2)
  # 
  # est <- rbind(est,
  #              c("n",nobs(mod)),
  #              c("Residual Deviance",dev),
  #              c("AIC", aic))
  
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
               "Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1. Estimated MRR with a p-value below 0.05 are in bold.")

names(est_all) <- c("Variable","Adj. MR All Causes", "Adj. MR CDP","Adj. MR CVD",
                    "Adj. MR RSP","Adj. MR CAN","Adj. MR LCA","Adj. MR Ext. Causes")


# Function to extract p-value from text, return boolean if it is below 0.05
f_extractPvalue <- function(text){
  text_return <- text %>% str_extract("\\(\\d\\.\\d\\d\\)") %>% 
    str_remove_all("\\(|\\)") %>% as.numeric()
  
  return(
    if_else(is.na(text_return)|text_return>0.05,F,T)
  )
}

est_all$`Adj. MR All Causes` %>% f_extractPvalue()

slice


est_all %>% 
  mutate(Variable=Variable %>% 
           str_remove_all("scale|\\(|\\)|log") %>% 
           f_replaceVar()) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1, align = "left", part="all") %>% 
  bold(j=1, bold=T) %>% 
  flextable::border(part="body",i=13,
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=1:2, value=as_paragraph(foot_note), part="header", inline=F,
           ref_symbols = c("a","b")) %>% 
  footnote(j=1,i=1, ref_symbols = c("c"),part="body", inline=F,
           value=as_paragraph("For PM2.5 MRR represent an increase in RR per 10 ug/m3")) %>% 
  footnote(j=1,i=2, ref_symbols = c("d"),part="body", inline=F,
           value=as_paragraph("For every other variable MRR is presented per an increase in one standard deviation from the mean")
           ) %>% 
  bold(j=2, i=f_extractPvalue(est_all$`Adj. MR All Causes`)) %>% 
  bold(j=3, i=f_extractPvalue(est_all$`Adj. MR CDP`)) %>% 
  bold(j=4, i=f_extractPvalue(est_all$`Adj. MR CVD`)) %>% 
  bold(j=5, i=f_extractPvalue(est_all$`Adj. MR RSP`)) %>% 
  bold(j=6, i=f_extractPvalue(est_all$`Adj. MR CAN`)) %>% 
  bold(j=7, i=f_extractPvalue(est_all$`Adj. MR LCA`)) %>% 
  bold(j=8, i=f_extractPvalue(est_all$`Adj. MR Ext. Causes`))
  # print(preview="docx")

# EoF