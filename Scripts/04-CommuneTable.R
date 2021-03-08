### MortalityRR-PM2.5
## Commune Table Sorted
## PBH Nov 2020

## Table for communes sorted on PM2.5

# Filter data and sort
df <- data_model %>% 
  filter(commune_valid) %>% 
  arrange(desc(mp25)) %>% 
  mutate(Latitude=map_dbl(geometry, ~st_centroid(.x)[[2]]))


# Select variables to shown
df <- df %>% 
  dplyr::select(nombre_comuna, region,Latitude, mp25, population,
                mrAdj_AllCauses, mrAdj_CDP,
                income_median_usd, perc_woodHeating) %>% 
  mutate(population=population/1e3)


# Top and bottom
n <- 10
df <- df[c(1:n,(nrow(df)-n+1):(nrow(df))),]
# df <- df %>% filter(nombre_comuna %in% c("Santiago","Calama"))

  # Difference between tops (weighted mean)
df %>% mutate(top10=mp25>20) %>% group_by(top10) %>% 
  summarise(mrAdj_CDP=weighted.mean(mrAdj_CDP, population, na.rm=T),
            mrAdj_AllCauses=weighted.mean(mrAdj_AllCauses, population, na.rm=T)) %>% ungroup()


# Change names
names(df) <- names(df) %>% f_replaceVar() %>% 
  str_replace_all("Commune name","Commune") %>% 
  str_replace_all("Latitude","Latitude (S)") %>%
  str_replace_all("Population 2017","Population 2017 [thousands]") %>% 
  str_replace_all("Adjusted mortality rate Cardiopulmonary","Adj. MR CDP") %>% 
  str_replace_all("Adjusted mortality rate All Causes","Adj. MR All Causes")

## Table
df %>% 
  flextable() %>% 
  bold(bold=T, part="header") %>% bold(j=1, bold=T) %>% 
  autofit(add_w = 0.1, add_h = 0.3) %>%
  align(j=1:2, align = "left", part="all") %>% 
  align(j=3:9, align = "right", part="all") %>% 
  colformat_num(j=c(3,4,5), digits=1) %>%
  colformat_num(j=c(6,7,8,9), digits=0) %>%
  flextable::border(i=10, part="body",
                    border.bottom = officer::fp_border(style = "solid", width=2))
# print(preview="pptx")
# print(preview="docx")

# Number of PM2.5 repeated
data_model$mp25 %>% unique() %>% length()
data_model %>% group_by(mp25) %>% summarise(count=n()) %>% arrange(desc(count))

## EoF