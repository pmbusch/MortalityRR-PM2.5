### MortalityRR-PM2.5
## Descriptive Table
## PBH Nov 2020
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
load('.RData')
source("Scripts/00-Functions.R", encoding = "UTF-8")


## Relevant indicators ------------
# Total population in commune with PM2.5 data
df_modelo %>% filter(!is.na(mp25)) %>% nrow()
total_pob <- df_modelo$poblacion %>% sum()
pob_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% pull(poblacion) %>% sum()
cat(round(pob_mp25/total_pob*100,1),
    "% population in communes with PM2.5 exposure estimated")


df_modelo <- df_modelo %>% 
  mutate(MR_allCauses=def_allCauses/poblacion*1e5,
         MR_CVD=def_cardio/poblacion*1e5,
         MR_pulmonary=def_pulmonar/poblacion*1e5,
         MR_cardioPulmonary=def_cardioPulmonar/poblacion*1e5,
         MR_cancer=def_cancer/poblacion*1e5)


# TABLE ALL COMMUNES: With and without PM2.5 ---------------
df_modelo %>% names()
df <- df_modelo %>% 
  mutate(perc_fonasa=perc_fonasa_A+perc_fonasa_B+perc_fonasa_C+perc_fonasa_D,
         poblacion=poblacion/1e3,
         ingresoTotal_mediana=ingresoTotal_mediana/1e3) %>% 
  select(MR_allCauses,MR_cardioPulmonary,MR_CVD,MR_pulmonary,MR_cancer, 
         mp25, mp10,
         poblacion,densidad_pob_censal, `15-44`, `45-64`, `65+`,perc_mujer, 
         perc_rural, perc_puebloOrig,perc_vivHacMedio,
         tasa_camas,
         ingresoTotal_mediana,perc_menor_media, perc_ocupado, perc_salud,
         perc_isapre, perc_fonasa_D,perc_fonasa_C,
         perc_fonasa_B, perc_fonasa_A, 
         perc_lenaCocina,perc_lenaCalefaccion,perc_lenaAgua,
         hr_summer, hr_winter, tmed_summer, tmed_winter,
         heating_degree_15_summer, heating_degree_15_winter)

# separo por la condicion de si tiene o no estacion
df_sep <- df %>% 
  mutate(has_monitor=if_else(!is.na(mp25),"si","no")) %>% 
  group_by(has_monitor) %>% skim() %>% 
  mutate(indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, has_monitor, indicator) %>% 
  spread(has_monitor, indicator)
  
df_skim <- df %>% skim() %>% 
  mutate(n=complete_rate*nrow(df),
         indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable,n, indicator) %>% 
  left_join(df_sep)

n_mp25 <- df_modelo %>% filter(!is.na(mp25)) %>% nrow()
foot_note <- paste("n:",c(nrow(df_modelo),nrow(df_modelo)-n_mp25,
                          n_mp25),"communes",sep=" ")

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Type=f_addTypeVar(skim_variable),
         skim_variable=f_replaceVar(skim_variable) %>% 
           str_replace("Population","Population [thousands]") %>% 
           str_replace("Median monthly income per capita",
                       "Median monthly income per capita [thousands $CLP]") %>% 
           str_replace_all("\\) \\[per 100,000\\]","\\)")) %>% 
  arrange(Type)

df_skim <- df_skim[,c(6,1,2,3,4,5)] # Reorder columns

df_skim %>% 
  rename(Variable=skim_variable, Total=indicator, 
         `Without PM2.5`=no, `With PM2.5`=si) %>% 
  mutate(Type=Type %>% str_replace("Mortality","Mortality \n [per 100,000]")) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "center", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:6, part="body",i=c(5,7,10,19,29),
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
  filter(!is.na(mp25)) %>% 
  skim() %>% 
  mutate(
    # n=complete_rate*nrow(filter(df,!is.na(mp25))) %>% round(0),
         cv=numeric.sd/numeric.mean) %>% 
  select(skim_variable, numeric.mean, cv,
         numeric.p0, numeric.p50, numeric.p100) %>% 
  rename(Variable=skim_variable,
         Promedio=numeric.mean,
         `C.V.`=cv,
         Min=numeric.p0, Mediana=numeric.p50, Max=numeric.p100)

# Cambio nombre variables
df_skim <- df_skim %>% 
  mutate(Type=f_addTypeVar(Variable),
         Variable=f_replaceVar(Variable) %>% 
           str_replace("Population","Population [thousands]") %>% 
           str_replace("Median monthly income per capita",
                       "Median monthly income per capita [thousands $CLP]") %>% 
         str_replace_all("\\) \\[per 100,000\\]","\\)")) %>% 
  arrange(Type)
df_skim <- df_skim[,c(7,1,2,3,4,5,6)] # Reorder columns


df_skim %>% 
  mutate(Type=Type %>% str_replace("Mortality","Mortality \n [per 100,000]")) %>% 
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
  flextable::border(j=2:7, part="body",i=c(5,7,10,19,29),
         border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=4, value=as_paragraph("Coefficient of Variation"), 
           part="header", inline=T)
  # print(preview="docx")
  # print(preview="pptx")


# TABLE MEAN (SD) FOR PM2.5 LEVELS ---------------
## Level set at 20 (Chilean Standard)
df <- df %>% filter(!is.na(mp25))

# Split for 20 ug condition
df_sep <- df %>% 
  mutate(above_standard=if_else(mp25>20,"si","no")) %>% 
  group_by(above_standard) %>% skim() %>% 
  mutate(indicator=paste(round(numeric.mean,1)," (",
                         round(numeric.sd,1),")", sep="")) %>% 
  select(skim_variable, above_standard, indicator) %>% 
  spread(above_standard, indicator)

df_skim <- df %>% skim() %>% 
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
           str_replace("Population","Population [thousands]") %>% 
           str_replace("Median monthly income per capita",
                       "Median monthly income per capita [thousands $CLP]") %>% 
           str_replace_all("\\) \\[per 100,000\\]","\\)")) %>% 
  arrange(Type)

df_skim <- df_skim[,c(5,1,2,3,4)] # Reorder columns

df_skim %>% 
  rename(Variable=skim_variable, Total=indicator, 
         `Below 20 ug/m3`=no, `Above 20 ug/m3`=si) %>% 
  mutate(Type=Type %>% str_replace("Mortality","Mortality \n [per 100,000]")) %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1:2, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3, align = "center", part="all") %>% 
  merge_v(j = 1) %>%   fix_border_issues(part = "all") %>% 
  flextable::border(j=1, part="body",
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  flextable::border(j=2:5, part="body",i=c(5,7,10,19,29),
                    border.bottom = officer::fp_border(style = "solid", width=2)) %>%
  footnote(j=3:5, value=as_paragraph(foot_note), part="header", inline=T,
           ref_symbols = c("a", "b", "c")) 
# print(preview="docx")
# print(preview="pptx")



## EoF