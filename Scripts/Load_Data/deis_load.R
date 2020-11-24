### MortalityRR-PM2.5
## Deis Data: Death certificates
## https://deis.minsal.cl/#datosabiertos
## PBH Nov 2020

# Load Excel
date_deis <- "29-10-2020"
df_deis <- read_delim(paste(
  "Data/Data_Original/DEIS/DEFUNCIONES_FUENTE_DEIS_2016_2020_",
  date_deis %>% str_remove_all("-"),".csv",sep=""),
                 delim = ";",col_names = T,
                 col_types = "dDcddccccccccccccccccccccc",
                 locale = locale(encoding = "windows-1252",
                                 date_format = "%Y-%m-%d"))
spec(df_deis)
names(df_deis) <- c("year","date","sexo","edad_tipo","edad",
                    "codigo_comuna","comuna","region",
                    "diag1","cap_diag1","glosa_cap_diag1",
                    "grupo_diag1","glosa_grupo_diag1",
                    "categ_diag1","glosa_categ_diag1",
                    "subcateg_diag1","glosa_subcateg_diag1",
                    "diag2","cap_diag2","glosa_cap_diag2",
                    "grupo_diag2","glosa_grupo_diag2",
                    "categ_diag2","glosa_categ_diag2",
                    "subcateg_diag2","glosa_subcateg_diag2")
                    # "causa_cie10","causa","cap_cie10","capitulo")

## Add year: antes estaba y lo borarron, como cambian el formato semana a semana....
## Volvieron a agregarlo, les gusta cambiar el formato cada vez....
# df_deis <- df_deis %>% mutate(year=year(date))

# Join with commune code
df_deis <- df_deis %>% filter(codigo_comuna!="99999") %>% 
  mutate(codigo_comuna=paste(
  if_else(str_length(codigo_comuna)==4,"0",""),codigo_comuna,sep=""))

# Unify Age into years
df_deis %>% group_by(edad_tipo) %>% summarise(count=n()) %>% arrange(desc(count))
df_deis <- df_deis %>% mutate(edad=if_else(edad_tipo!=1,0,edad),
                              edad_tipo=1)

# Factor
df_deis <- df_deis %>% mutate(sexo=factor(sexo),
                    glosa_subcateg_diag1=factor(glosa_subcateg_diag1))

df_deis$edad %>% unique()
df_deis <- df_deis %>% mutate(grupo_edad=case_when(
  edad < 15 ~ "0-14",
  edad < 45 ~ "15-44",
  edad < 65 ~ "45-64",
  T ~ "65+"))

# Filter for 2017-2019
df_deis <- df_deis %>% filter(year %in% c(2017:2019))


## EoF