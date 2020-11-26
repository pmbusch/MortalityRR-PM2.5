### MortalityRR-PM2.5
## Load MINVU Data
## MINVU - Ministry of housing and urbanism
## https://www.observatoriourbano.cl/estadisticas-habitacionales/
## Index of overcrowding in Housing (habitants per household). Estimated from census data from 2017.
## PBH Nov 2020



## overcrowding ---------------
## Load data
## Categories:
# "Viviendas sin Hacinamiento (menos de 2,5 personas por dormitorio)"	
# "Viviendas con Hacinamiento Medio (entre 2,5 y menos de 5 personas por dormitorio)"	
# "Viviendas con Hacinamiento Crítico (más de 5 personas por dormitorio o sin dormitorio)"	
# "Viviendas donde no se reporta cantidad de dormitorios (hacinamiento ignorado)"	

df_minvu <- read_excel("Data/Data_Original/DD Hacinamiento Comuna Urbano y Rural CENSO 2017.xlsx",
                       sheet="Vivienda Hacinamiento",
                       range = "A26:R371",
                       col_names = c("region","comuna","codigo_comuna",
                                     "viv_urb","viv_rural","viv_total",
                                     "viv_sinHac_urb","viv_sinHac_rural","viv_sinHac_total",
                                     "viv_HacMedio_urb","viv_HacMedio_rural","viv_HacMedio_total",
                                     "viv_HacCritico_urb","viv_HacCritico_rural","viv_HacCritico_total",
                                     "viv_sinDorm_urb","viv_sinDorm_rural","viv_sinDorm_total"))


## Add a 0 to commune coddes
df_minvu <- df_minvu %>% 
  mutate(codigo_comuna=paste(if_else(str_length(codigo_comuna)==4,"0",""),
                      codigo_comuna,sep=""))


## EoF