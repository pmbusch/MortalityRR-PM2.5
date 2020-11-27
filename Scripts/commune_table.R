### MortalityRR-PM2.5
## Commune Table Sorted
## PBH Nov 2020

## Table for communes sorted on PM2.5

# Filter data and sort
df <- data_model %>% 
  filter(!is.na(mp25)) %>% 
  arrange(desc(mp25)) %>% 
  mutate(Latitud=map_dbl(geometry, ~st_centroid(.x)[[2]]))


# Select variables to shown
df <- df %>% 
  select(nombre_comuna, region,Latitud, mp25, population, mrAdj_CDP, mrAdj_CAN, 
         income_median, perc_woodHeating) %>% 
  mutate(population=population/1e3,
         income_median=income_median/1e3)


# Top and bottom
n <- 10
df <- df[c(1:n,(nrow(df)-n+1):(nrow(df))),]


# Difference between tops (weighted mean)
df %>% mutate(top10=mp25>20) %>% group_by(top10) %>% 
  summarise(mrAdj_CDP=weighted.mean(mrAdj_CDP, population, na.rm=T),
            mrAdj_CAN=weighted.mean(mrAdj_CAN, population, na.rm=T)) %>% ungroup()


# Change names
names(df) <- names(df) %>% f_replaceVar() %>% 
  str_replace_all("Commune name","Commune") %>% 
  str_replace_all("Population","Population [thousands]") %>% 
  str_replace_all("Adjusted mortality rate Cardiopulmonary","Adj. MR CDP") %>% 
  str_replace_all("Adjusted mortality rate Cancer","Adj. MR CAN") %>% 
  str_replace_all("Median monthly income per capita",
                  "Median monthly income per capita [thousands $CLP]")

## Table
df %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3:9, align = "right", part="all") %>% 
  colformat_num(j=3:9, digits=2) %>% 
  flextable::border(i=10, part="body",
                    border.bottom = officer::fp_border(style = "solid", width=2))
# print(preview="pptx")
# print(preview="docx")


## EoF