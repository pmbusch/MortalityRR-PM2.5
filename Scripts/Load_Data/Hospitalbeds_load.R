### MortalityRR-PM2.5
## Number of hospital beds per medical compound at 2019
# Source : https://deis.minsal.cl/#datosabiertos Listado de Establecimientos de Salud
## PBH Nov 2020


## Public Beds -------------
df_camas_pub <- read_excel("Data/Data_Original/Establecimientos_ChileDEIS_MINSAL(10-07-2020).xlsx",
                       sheet = "Dotación de Camas Públicas")

# line 2 has the column names
names(df_camas_pub) <- df_camas_pub[2,] %>% str_to_lower() %>% str_replace_all(" ","_") %>% 
  str_replace_all("á","a") %>% str_replace_all("ó","o")

# erase 3 first lines
df_camas_pub <- df_camas_pub[-1:-3,]

# erase ifo of Area funcional of bed
df_camas_pub <- df_camas_pub %>% filter(!is.na(codigo_establecimiento)) %>% 
  mutate(total=as.numeric(total),
         tipo_est="Publico")

# total public beds 26,382
df_camas_pub$total %>% sum()

## Private beds -------------
df_camas_priv <- read_excel("Data/Data_Original/Establecimientos_ChileDEIS_MINSAL(10-07-2020).xlsx",
                           sheet = "Dotación de Camas Privadas")

# line 2 has the column names
names(df_camas_priv) <- df_camas_priv[2,] %>% str_to_lower() %>% str_replace_all(" ","_") %>% 
  str_replace_all("á","a") %>% str_replace_all("ó","o")

# erase 3 first lines
df_camas_priv <- df_camas_priv[-1:-3,]

# erase ifo of Area funcional of bed
df_camas_priv <- df_camas_priv %>% filter(!is.na(codigo_establecimiento)) %>% 
  mutate(total=as.numeric(total_2019),
         total_2019=NULL,
         tipo_est="Privado")

# total private beds 11,916
df_camas_priv$total %>% sum()

## Codigos Establecimientos -----------
# To join medical compound (establecimiento) with the commune
df_establecimiento <- read_excel("Data/Data_Original/Establecimientos_ChileDEIS_MINSAL(10-07-2020).xlsx",
                            sheet = "Establecimientos Vigentes")

# line 1 has the column names
names(df_establecimiento) <- df_establecimiento[1,] %>% str_to_lower() %>% str_replace_all(" ","_") %>% 
  str_replace_all("á","a") %>% str_replace_all("ó","o")
# erase first line
df_establecimiento <- df_establecimiento[-1,]

# rename variables
df_establecimiento <- df_establecimiento %>% 
  rename(longitud=`longitud_[grados_decimales]`,
         latitud=`latitud______[grados_decimales]`) %>% 
  mutate(longitud=as.numeric(longitud),
         latitud=as.numeric(latitud))


# Join data
df_camas <- rbind(
  df_camas_pub %>% select(codigo_establecimiento, nombre_establecimiento,
                          total, tipo_est),
  df_camas_priv %>% select(codigo_establecimiento, nombre_establecimiento,
                           total, tipo_est))

df_camas$total %>% sum() # Total beds 38,298


## Join with commune, based on compound code
df_camas <- left_join(df_camas, 
                      df_establecimiento %>% select(codigo_nuevo, codigo_comuna,
                                                    longitud,latitud),
                      by=c("codigo_establecimiento"="codigo_nuevo"))

## EoF