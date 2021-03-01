### MortalityRR-PM2.5
## Interactive maps to visualize commune data
## PBH Feb 2021


# Data ----
# devtools::install_github("r-spatial/mapview@develop")
library(RColorBrewer)

data_model %>% names()
mapa <- left_join(map_commune, data_model, by=c("codigo_comuna")) %>% 
  rename(region=region.x)

library(leaflet)
library(htmltools)
library(scales)


## OVERKILL 2: Leaflet
# Function to plot maps
f_leafleft <- function(map, datos,var, columna, unidad){
  pal <- colorBin("YlOrRd", bins = 9, domain=datos %>% pull(columna))
  labels <- sprintf(
    "<strong>%s</strong><br/> %s: %s [%s]",
    datos$nombre_comuna, columna,
    comma(datos %>% pull(columna),0.1), unidad) %>% 
    lapply(HTML)
  
  map %>% 
    addPolygons(
    group = columna,
    # fill
    fillColor   = ~pal({{var}}),
    fillOpacity = 0.7,
    # line
    dashArray   = "3",
    weight      = 0.1,
    color       = "white",
    opacity     = 1,
    # interaction
    highlight = highlightOptions(
      weight = 1,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
    addLegend(
      group = columna,
      pal = pal, 
      values = ~{{var}}, opacity = 0.7, 
      title = HTML(paste(columna, " [",unidad,"]",sep="")),
      position = "bottomleft")
}


mapa_filter <- mapa %>% 
  dplyr::select(region, nombre_provincia, nombre_comuna, population,
                mp25, mp25_winter,
                mr_AllCauses_allAges, mrAdj_AllCauses,
                mr_CDP_allAges,mrAdj_CDP,
                mr_RSP_allAges, mrAdj_RSP,
                mr_CVD_allAges, mrAdj_CVD,
                mr_CAN_allAges, mrAdj_CAN,
                mr_LCA_allAges, mrAdj_LCA,
                mr_ExtCauses_allAges, mrAdj_ExtCauses,
                urbanDensity,urbanDensity_mean,urbanDensity_median,
                urbanDensity_mean_p90,
                age_0_14, age_15_44, age_45_64, age_65plus,age_75plus, 
                age_30plus, perc_female,
                perc_rural, perc_ethnicityOrig, 
                perc_overcrowding_medium,perc_overcrowding_high,
                income_mean,income_median, 
                perc_less_highschool, perc_isapre,perc_fonasa_A,
                perc_fonasa_B,perc_fonasa_C,perc_fonasa_D,
                perc_woodCooking, perc_woodHeating, perc_woodWarmWater,perc_wood_avg,
                hr_anual, tmed_anual,
                tmed_summer,tmed_winter,hr_summer,hr_winter,heating_degree_15_summer,
                heating_degree_15_winter, heating_degree_18_winter)

# Map -----------
m_leaf <- leaflet(mapa_filter) %>% 
  addTiles() %>% 
  f_leafleft(mapa_filter,mapa_filter$population,
             "population", unidad = "hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mp25,
             "mp25", unidad = "ug/m3") %>% 
  f_leafleft(mapa_filter,mapa_filter$mp25_winter,
             "mp25_winter", unidad = "ug/m3") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_AllCauses_allAges,
             "mr_AllCauses_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_AllCauses,
             "mrAdj_AllCauses", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_CDP_allAges,
             "mr_CDP_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_CDP,
             "mrAdj_CDP", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_RSP_allAges,
             "mr_RSP_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_RSP,
             "mrAdj_RSP", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_CVD_allAges,
             "mr_CVD_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_CVD,
             "mrAdj_CVD", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_CAN_allAges,
             "mr_CAN_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_CAN,
             "mrAdj_CAN", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_LCA_allAges,
             "mr_LCA_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_LCA,
             "mrAdj_LCA", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mr_ExtCauses_allAges,
             "mr_ExtCauses_allAges", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$mrAdj_ExtCauses,
             "mrAdj_ExtCauses", unidad = "deaths/100K hab") %>% 
  f_leafleft(mapa_filter,mapa_filter$urbanDensity,
             "urbanDensity", unidad = "hab/km2") %>% 
  f_leafleft(mapa_filter,mapa_filter$urbanDensity_mean,
             "urbanDensity_mean", unidad = "hab/km2") %>% 
  f_leafleft(mapa_filter,mapa_filter$urbanDensity_median,
             "urbanDensity_median", unidad = "hab/km2") %>% 
  f_leafleft(mapa_filter,mapa_filter$urbanDensity_mean_p90,
             "urbanDensity_mean_p90", unidad = "hab/km2") %>% 
  f_leafleft(mapa_filter,mapa_filter$age_0_14,
             "age_0_14", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$age_15_44,
             "age_15_44", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$age_45_64,
             "age_45_64", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$age_65plus,
             "age_65plus", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$age_75plus,
             "age_75plus", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$age_30plus,
             "age_30plus", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_female,
             "perc_female", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_rural,
             "perc_rural", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_ethnicityOrig,
             "perc_ethnicityOrig", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_overcrowding_medium,
             "perc_overcrowding_medium", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_overcrowding_high,
             "perc_overcrowding_high", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$income_mean,
             "income_mean", unidad = "CLP month") %>% 
  f_leafleft(mapa_filter,mapa_filter$income_median,
             "income_median", unidad = "CLP month") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_less_highschool,
             "perc_less_highschool", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_isapre,
             "perc_isapre", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_fonasa_A,
             "perc_fonasa_A", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_fonasa_D,
             "perc_fonasa_D", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_woodCooking,
             "perc_woodCooking", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_woodHeating,
             "perc_woodHeating", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_woodWarmWater,
             "perc_woodWarmWater", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$perc_wood_avg,
             "perc_wood_avg", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$tmed_anual,
             "tmed_anual", unidad = "°C") %>% 
  f_leafleft(mapa_filter,mapa_filter$tmed_summer,
             "tmed_summer", unidad = "°C") %>% 
  f_leafleft(mapa_filter,mapa_filter$tmed_winter,
             "tmed_winter", unidad = "°C") %>% 
  f_leafleft(mapa_filter,mapa_filter$hr_anual,
             "hr_anual", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$hr_summer,
             "hr_summer", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$hr_winter,
             "hr_winter", unidad = "%") %>% 
  f_leafleft(mapa_filter,mapa_filter$heating_degree_15_summer,
             "heating_degree_15_summer", unidad = "°C") %>% 
  f_leafleft(mapa_filter,mapa_filter$heating_degree_15_winter,
             "heating_degree_15_winter", unidad = "°C") %>% 
  f_leafleft(mapa_filter,mapa_filter$heating_degree_18_winter,
             "heating_degree_18_winter", unidad = "°C") %>% 
  addLayersControl(
    baseGroups = c("OpenStreetMap","Toner", "Toner by Stamen"),
    overlayGroups = c("population",
                      "mp25", "mp25_winter",
                      "mr_AllCauses_allAges", "mrAdj_AllCauses",
                      "mr_CDP_allAges","mrAdj_CDP",
                      "mr_RSP_allAges", "mrAdj_RSP",
                      "mr_CVD_allAges", "mrAdj_CVD",
                      "mr_CAN_allAges", "mrAdj_CAN",
                      "mr_LCA_allAges", "mrAdj_LCA",
                      "mr_ExtCauses_allAges", "mrAdj_ExtCauses",
                      "urbanDensity","urbanDensity_mean",
                      "urbanDensity_median",
                      "urbanDensity_mean_p90",
                      "age_0_14", "age_15_44", "age_45_64",
                      "age_65plus","age_75plus", 
                      "age_30plus", "perc_female",
                      "perc_rural", "perc_ethnicityOrig", 
                      "perc_overcrowding_medium","perc_overcrowding_high",
                      "income_mean","income_median", 
                      "perc_less_highschool", "perc_isapre","perc_fonasa_A",
                      "perc_fonasa_B","perc_fonasa_C","perc_fonasa_D",
                      "perc_woodCooking", "perc_woodHeating", 
                      "perc_woodWarmWater","perc_wood_avg",
                      "tmed_anual",
                      "tmed_summer","tmed_winter",
                      "hr_anual", 
                      "hr_summer","hr_winter",
                      "heating_degree_15_summer",
                      "heating_degree_15_winter", "heating_degree_18_winter")) %>% 
  hideGroup(c("population",
              "mp25", "mp25_winter",
              "mr_AllCauses_allAges", "mrAdj_AllCauses",
              "mr_CDP_allAges","mrAdj_CDP",
              "mr_RSP_allAges", "mrAdj_RSP",
              "mr_CVD_allAges", "mrAdj_CVD",
              "mr_CAN_allAges", "mrAdj_CAN",
              "mr_LCA_allAges", "mrAdj_LCA",
              "mr_ExtCauses_allAges", "mrAdj_ExtCauses",
              "urbanDensity","urbanDensity_mean",
              "urbanDensity_median",
              "urbanDensity_mean_p90",
              "age_0_14", "age_15_44", "age_45_64",
              "age_65plus","age_75plus", 
              "age_30plus", "perc_female",
              "perc_rural", "perc_ethnicityOrig", 
              "perc_overcrowding_medium","perc_overcrowding_high",
              "income_mean","income_median", 
              "perc_less_highschool", "perc_isapre","perc_fonasa_A",
              "perc_fonasa_B","perc_fonasa_C","perc_fonasa_D",
              "perc_woodCooking", "perc_woodHeating", 
              "perc_woodWarmWater","perc_wood_avg",
              "tmed_anual",
              "tmed_summer","tmed_winter",
              "hr_anual", 
              "hr_summer","hr_winter",
              "heating_degree_15_summer",
              "heating_degree_15_winter", "heating_degree_18_winter"))
# m_leaf

# Save as html
mapshot(m_leaf, "Paper_escritura/Supplementary/MapCommuneVariables.html", 
        selfcontained=F)

# library(leafsync)
# 
# m_leaf_dual <- sync(m_leaf,m_leaf, ncol=2)
# mapshot(m_leaf_dual, "Paper_escritura/Supplementary/DualMapCommuneVar.html", 
#         selfcontained=F)
# 
# rm(m_leaf,m_leaf_dual)


## EoF