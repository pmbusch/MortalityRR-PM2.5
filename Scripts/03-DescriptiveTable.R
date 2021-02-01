### MortalityRR-PM2.5
## Descriptive Table
## PBH Nov 2020
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load('.RData')
source("Scripts/00-Functions.R", encoding = "UTF-8")


## Relevant indicators ------------
# Total population in commune with PM2.5 data (use data valid for the model)
data_model %>% filter(commune_valid) %>% nrow()
total_pob <- data_model$population %>% sum()
pob_mp25 <- data_model %>% filter(commune_valid) %>% pull(population) %>% sum()
cat(round(pob_mp25/total_pob*100,1),
    "% population in communes with PM2.5 exposure estimated")


# TABLE ALL COMMUNES: With and without PM2.5 ---------------
data_model %>% names()
df <- data_model %>% 
  mutate(perc_fonasa=perc_fonasa_A+perc_fonasa_B+perc_fonasa_C+perc_fonasa_D,
         population=population/1e3,
         income_median=income_median/1e3) %>% 
  select(mrAdj_AllCauses,mrAdj_CDP,mrAdj_CVD,mrAdj_RSP,mrAdj_CAN,mrAdj_LCA, mrAdj_ExtCauses, 
         mp25,commune_valid,
         population,urbanDensity, age_15_44, age_45_64, age_65plus,perc_female, 
         perc_rural, perc_ethnicityOrig,
         perc_overcrowding_medium,perc_overcrowding_high,
         income_median_usd,perc_less_highschool, perc_occupancy,
         perc_isapre, perc_fonasa_CD, perc_fonasa_AB, 
         perc_woodCooking,perc_woodHeating,perc_woodWarmWater,
         hr_summer, hr_winter, tmed_summer, tmed_winter,
         heating_degree_15_summer, heating_degree_15_winter)

# separo por la condicion de si tiene o no estacion
df_sep <- df %>% 
  mutate(has_monitor=if_else(commune_valid,"si","no")) %>% 
  group_by(has_monitor) %>% skim() %>% 
  mutate(indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep=""),
         commune_valid=NULL) %>% 
  select(skim_variable, has_monitor, indicator) %>% 
  spread(has_monitor, indicator)
  
df_skim <- df %>% 
  select(-commune_valid) %>% 
  skim() %>% 
  mutate(n=complete_rate*nrow(df),
         indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable,n, indicator) %>% 
  left_join(df_sep)

n_mp25 <- data_model %>% filter(commune_valid) %>% nrow()
foot_note <- paste("n:",c(nrow(data_model),nrow(data_model)-n_mp25,
                          n_mp25),"communes",sep=" ")

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Type=f_addTypeVar(skim_variable),
         skim_variable=f_replaceVar(skim_variable) %>% 
           str_replace("Population 2017","Population 2017 [thousands]") %>% 
           str_replace_all("\\) \\[per 100,000\\]","\\)") %>% 
           str_remove_all("Adjusted mortality rate")) %>% 
  arrange(Type)

df_skim <- df_skim[,c(6,1,2,3,4,5)] # Reorder columns

df_skim %>% 
  rename(Variable=skim_variable, Total=indicator, 
         `Without PM2.5`=no, `With PM2.5`=si) %>% 
  mutate(Type=Type %>% str_replace("Mortality","Adjusted mortality \n rate [per 100,000] \n 2017-2019")) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "center", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:6, part="body",i=c(7,8,11,21,27),
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=4:6, value=as_paragraph(foot_note), part="header", inline=T)
# print(preview="pptx")
# print(preview="docx")

rm(foot_note,df_skim, df_sep, n_mp25)

## DESCRIPTIVE TABLE FOR PM2.5 COMMUNES -------------
# Includes Coefficient of variation: see more clearly wich variables hold the highest relative variation
# Note: Only valid for variables with a meaningful zero (as ratios). Not valid for T°
# https://en.wikipedia.org/wiki/Coefficient_of_variation
df_skim <- df %>% 
  filter(commune_valid) %>% select(-commune_valid) %>% 
  skim() %>% 
  mutate(
    # n=complete_rate*nrow(filter(df,!is.na(mp25))) %>% round(0),
         cv=numeric.sd/numeric.mean) %>% 
  select(skim_variable, numeric.mean, numeric.sd,
         numeric.p0, numeric.p50, numeric.p100) %>% 
  rename(Variable=skim_variable,
         Mean=numeric.mean,
         `Std. Dev.`=numeric.sd,
         Min=numeric.p0, Median=numeric.p50, Max=numeric.p100)

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Type=f_addTypeVar(Variable),
         Variable=f_replaceVar(Variable) %>% 
           str_replace("Population 2017","Population 2017 [thousands]") %>% 
         str_replace_all("\\) \\[per 100,000\\]","\\)") %>% 
           str_remove("\\[per 100,000\\]") %>% 
           str_remove_all("Adjusted mortality rate")) %>% 
  arrange(Type)
df_skim <- df_skim[,c(7,1,2,3,4,5,6)] # Reorder columns


df_skim %>% 
  mutate(Type=Type %>% str_replace("Mortality","Adjusted mortality \n rate [per 100,000] \n 2017-2019")) %>% 
  flextable() %>% 
  colformat_num(big.mark=" ", digits=1, j=3:ncol(df_skim),
                na_str="s/i") %>%
  colformat_num(big.mark=" ", digits=0, j=c(3,5:ncol(df_skim)),
                i=c(2,6,7,11,15,16,25,26), na_str="s/i") %>%
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:7, part="body",i=c(7,8,11,21,27),
         border.bottom = officer::fp_border(style = "solid", width=2))
  # print(preview="docx")
  # print(preview="pptx")


# TABLE MEAN (SD) FOR PM2.5 LEVELS ---------------
## Level set at 20 (Chilean Standard)
df <- df %>% filter(commune_valid)

# Split for 20 ug condition
df_sep <- df %>% 
  select(-commune_valid) %>% 
  mutate(above_standard=if_else(mp25>20,"si","no")) %>% 
  group_by(above_standard) %>% skim() %>% 
  mutate(indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, above_standard, indicator) %>% 
  spread(above_standard, indicator)

df_skim <- df %>% 
  select(-commune_valid) %>% 
  skim() %>% 
  mutate(n=complete_rate*nrow(df),
         indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, indicator) %>% 
  left_join(df_sep)

n_mp25 <- df %>% filter(mp25>20) %>% nrow()
foot_note <- paste("n:",c(nrow(df),nrow(df)-n_mp25,
                          n_mp25),"communes",sep=" ")

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Type=f_addTypeVar(skim_variable),
         skim_variable=f_replaceVar(skim_variable) %>% 
           str_replace("Population 2017","Population 2017 [thousands]") %>% 
           str_replace_all("\\) \\[per 100,000\\]","\\)") %>% 
           str_remove_all("Adjusted mortality rate")) %>% 
  arrange(Type)

df_skim <- df_skim[,c(5,1,2,3,4)] # Reorder columns

df_skim %>% 
  rename(Variable=skim_variable, Total=indicator, 
         `Below 20 ug/m3`=no, `Above 20 ug/m3`=si) %>% 
  mutate(Type=Type %>% str_replace("Mortality","Adjusted mortality \n rate [per 100,000] \n 2017-2019")) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "center", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:5, part="body",i=c(7,8,11,21,27),
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=3:5, value=as_paragraph(foot_note), part="header", inline=T,
           ref_symbols = c("a", "b", "c")) 
# print(preview="docx")
# print(preview="pptx")


## EoF