### MortalityRR-PM2.5
## Air pollution time series analysis
## PBH Feb. 2020


theme_set(theme_bw(16)+theme(panel.grid.major = element_blank()))
file_name <- "Figures/TimeSeries/%s.png"
source("Scripts/00-Functions.R", encoding = "UTF-8")
source("Scripts/Load_Data/map_load.R", encoding = "UTF-8")

# Load raw data air pollution--------
df_conc <- read_rds("Data/Data_Model/AirPollution_Data_raw.rsd")


# Filter to selected period and PM2.5
df_conc <- df_conc %>% mutate(region=factor(region, levels_region)) %>% 
  filter(pollutant=="mp2.5" & year %in% 2010:2019)

# Number monitors with data
df_conc$site %>% unique() %>% length()
# Number of communes with data
df_conc$codigo_comuna %>% unique() %>% length()

## Number of monitors with data per year ------
df_anual <- df_conc %>% group_by(site,codigo_comuna, year) %>% 
  summarise(valor=mean(valor, na.rm=T),
            disponibilidad=n()/365) %>% ungroup()


df_anual <- df_anual %>% 
  filter(disponibilidad>0.8) %>%
  group_by(year) %>% 
  summarise(count=n()) %>% ungroup()

df_anual %>% 
  ggplot(aes(year, count))+
  geom_col(fill="brown")+
  geom_text(aes(label=count), size=10, vjust=-0.8)+
  scale_x_continuous(breaks=2010:2019)+
  coord_cartesian(ylim=c(0,85), expand=F)+
  labs(x="", y="Number of monitor sites with PM2.5 data")+
  theme_bw(28)+theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())

f_savePlot(last_plot(), sprintf(file_name,"NSite_years"))

rm(df_anual)

## 2017-2019 Monitor average (Bar graph) ---------
## Filter: avg 2017-2019, 80% data and the 3 years with data
# Monitor
df_avg <- df_conc %>% 
  filter(year %in% 2017:2019) %>% 
  group_by(site,region,codigo_comuna,nombre_comuna, year,latitud) %>% 
  summarise(valor=mean(valor,na.rm=T),
            disponibilidad=n()/365) %>% ungroup()
df_avg <- df_avg %>% 
  filter(disponibilidad>0.8) %>%
  group_by(site, region, codigo_comuna,nombre_comuna,latitud) %>% 
  summarise(valor=mean(valor, na.rm=T),
            count=n()) %>% ungroup() %>% 
  filter(count==3) %>% select(-count)

df_avg$valor %>% range()
df_avg %>% 
  mutate(highlight=if_else(valor>20,"yes","no"),
         estacion=if_else(
           str_detect(str_to_lower(site),str_to_lower(nombre_comuna)),
           paste(site,sep=""), 
           paste(nombre_comuna,site,sep="-"))) %>%
  ggplot(aes(x=reorder(estacion, latitud), y=valor, fill=highlight)) +
  geom_col()+
  geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  facet_grid(region~., scales = "free", space="free")+
  coord_flip(clip="off")+
  scale_fill_manual(values = c("#B0B0B0D0", "#BD3828D0"), guide = "none")+
  scale_x_discrete(name = NULL)+
  scale_y_continuous(name=expression(paste(
    "Annual average of PM2.5 2017-2019 [",mu,"g/",m^3,"]", sep="")),
                     expand = c(0, 0),
                     labels=function(x) format(x,big.mark = " ", decimal.mark = ".", scientific = F))+
  labs(caption= expression(paste(
    "Red line sets the national standard [20 ",mu,"g/",m^3,")",sep="")))+
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
# f_savePlot(last_plot(), sprintf(file_name,"Bar_Site"), dpi=600)
ggsave(sprintf(file_name,"Bar_Site"), last_plot(),dpi=600,
       width = 14.87, height = 12, units = "in")


## Heatmap -------
df_mes <- df_conc %>% 
  group_by(year,month,site,latitud,codigo_comuna) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste(year,month,1,sep="-") %>%  
           strptime(format="%Y-%m-%d") %>% 
           as_date()) %>% 
  left_join(map_commune)

df_mes %>% 
  ggplot(aes(x = date, y = reorder(site, latitud), fill = valor)) + 
  geom_tile() + 
  facet_grid(cod_region~., scales = "free", space="free")+
  scale_fill_distiller(palette = "YlOrRd", type = 'seq', 
                       na.value = "white", direction = 1,
                       name=expression(paste(
                         "Monthly avg. PM2.5 [",mu,"g/",m^3,"]",sep="")),
                       trans="sqrt",
                       guide = guide_legend(
                         label.position="bottom",
                         label.theme = element_text(angle = 45)),) + 
  scale_y_discrete(name = NULL)+
  scale_x_date(name="", date_breaks = "2 years",date_labels = "%Y")+
  coord_cartesian(expand=F)+
  theme(axis.text.y = element_text(size=8),
        legend.position = "bottom")

ggsave(sprintf(file_name,"HeatMap_Site"), last_plot(),dpi=600,
       width = 16, height = 12, units = "in")

# TIME SERIES key monitors ------------

key_sites <- c("Bomberos","Las Condes","Parque O'Higgins",
               "La Florida","Puente Alto",
               # "Rancagua I",
               "Nueva Libertad",
               "21 de mayo-Los Angeles","Las Encinas Temuco","Valdivia",
               "Osorno","Coyhaique")

## Scatter day series ----
df_day <- df_conc %>% 
  group_by(year,month,day,site,latitud,codigo_comuna) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste(year,month,day,sep="-") %>%  
           strptime(format="%Y-%m-%d") %>% 
           as_date()) %>% 
  left_join(map_commune) %>% 
  filter(site %in% key_sites) %>% 
  mutate(site_name=paste(zone,"-",site,""))

df_day %>% 
  ggplot(aes(x = date, y=valor,col = reorder(site_name, latitud))) + 
  geom_point(alpha=0.5)+
  facet_grid(zone~.)+
  ylab(expression(paste("PM2.5 [",mu,"g/",m^3,"]",sep="")))+
  labs(col="Site")+
  scale_x_date(name="",date_labels = "%Y", date_breaks = "2 year")+
  scale_color_discrete(guide = guide_legend(reverse = TRUE))+
  coord_cartesian(expand=F,
                  # ylim = c(0,150)
  )+
  theme_bw(20)+theme(panel.grid.major = element_blank(),
                     legend.text = element_text(size=12))

ggsave(sprintf(file_name,"TimeSeries_Day"), last_plot(),dpi=600,
       width = 16, height = 12, units = "in")


last_plot()+geom_smooth(method="loess",se=F, span=0.1)

## Monthly series -------
df_mes <- df_conc %>% 
  group_by(year,month,site,latitud,codigo_comuna) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste(year,month,1,sep="-") %>%  
           strptime(format="%Y-%m-%d") %>% 
           as_date()) %>% 
  left_join(map_commune)


# Filter monitor sites
df_mes_key <- df_mes %>% 
  filter(site %in% key_sites)
df_mes_key$site %>% unique()

# Add Region to name
df_mes_key <- df_mes_key %>% 
  mutate(site_name=paste(zone,"-",site,""))

df_mes_key$zone %>% unique()


df_mes_key %>% 
  ggplot(aes(x = date, y=valor,
             col = reorder(site_name, latitud), group= site_name)) + 
  geom_line()+geom_point()+
  # facet_wrap(~zone)+
  facet_grid(zone~.)+
  # scale_color_viridis_d()+
  ylab(expression(paste("Monthly avg. PM2.5 [",mu,"g/",m^3,"]",sep="")))+
  labs(col="Zone-Site")+
  scale_x_date(name="",date_labels = "%Y", date_breaks = "2 year")+
  scale_color_discrete(guide = guide_legend(reverse = TRUE))+
  coord_cartesian(expand=F,
                  ylim = c(0,150)
                  )+
  theme_bw(20)+theme(panel.grid.major = element_blank(),
                     legend.text = element_text(size=16))

ggsave(sprintf(file_name,"TimeSeries_Month"), last_plot(),dpi=600,
       width = 16, height = 12, units = "in")

df_mes_key %>% 
  ggplot(aes(x = date, y=valor,
             col = reorder(site_name, latitud), group= site_name)) + 
  geom_point()+
  geom_smooth(method="loess", se=F)+
  facet_grid(zone~.)+
  ylab(expression(paste("Monthly avg. PM2.5 [",mu,"g/",m^3,"]",sep="")))+
  labs(col="Zone-Site")+
  scale_x_date(name="",date_labels = "%Y", date_breaks = "2 year")+
  scale_color_discrete(guide = guide_legend(reverse = TRUE))+
  coord_cartesian(expand=F,
                  ylim = c(0,150)
  )+
  theme_bw(20)+theme(panel.grid.major = element_blank(),
                     legend.text = element_text(size=16))

ggsave(sprintf(file_name,"TimeSeries_Month_Loess"), last_plot(),dpi=600,
       width = 16, height = 12, units = "in")


# # https://stackoverflow.com/questions/14840542/place-a-legend-for-each-facet-wrap-grid-in-ggplot2
# library(gridExtra)
# 
# xs <- split(df_mes_key,f = df_mes_key$zone)
# p1 <- ggplot(xs$North,aes(x = date,y = valor,group = site_name,colour = site_name)) + 
#   geom_line() + geom_point()+
#   facet_grid(zone~.)+
#   ylab("Monthly avg. PM2.5 [ug/m3]")+labs(col="")+
#   ylab("")+
#   scale_x_date(name="",date_labels = "%Y", date_breaks = "2 year")+
#   scale_color_discrete(guide = guide_legend(reverse = TRUE))+
#   coord_cartesian(expand=F,
#                   ylim = c(0,150)
#   )+
#   theme_bw(20)+theme(panel.grid.major = element_blank())
# 
# p2 <- p1 %+% xs$Center
# p3 <- p1 %+% xs$Metropolitan
# p4 <- p1 %+% xs$South
# p5 <- p1 %+% xs$Patagonia
# 
# grid.arrange(p1,p2,p3,p4,p5, ncol=1)
# 
# # https://community.rstudio.com/t/unique-legends-in-facet-grid/49195
# out <- by(data = df_mes_key, INDICES = df_mes_key$zone, FUN = function(m) {
#   m <- droplevels(m)
#   m <- ggplot(data = m, 
#               aes(date, valor, group = site_name, colour = site_name)) + 
#     geom_line() + geom_point()+
#     facet_grid(zone~.)+
#     ylab("Monthly avg. PM2.5 [ug/m3]")+labs(col="")+
#     ylab("")+
#     scale_x_date(name="",date_labels = "%Y", date_breaks = "2 year")+
#     scale_color_discrete(guide = guide_legend(reverse = TRUE))+
#     coord_cartesian(expand=F,
#                     ylim = c(0,150)
#     )+
#     theme_bw(20)+theme(panel.grid.major = element_blank())
#   
# })
# do.call(cowplot::plot_grid, c(out, ncol = 1, align = 'v',
#                               labels = as.character(df_mes_key$zone),
#                               label_size = 20
# ))
# 
# cowplot::plot_grid(out$North, out$Center, out$Metropolitan,
#                    out$South, out$Patagonia, ncol=1, align="v")


## Annual series (slope graph)----

df_anual <- df_conc %>% 
  group_by(year,site,latitud,codigo_comuna) %>% 
  summarise(valor=mean(valor,na.rm=T)) %>% ungroup() %>% 
  mutate(date=paste(year,1,1,sep="-") %>%  
           strptime(format="%Y-%m-%d") %>% 
           as_date(),
         valor=round(valor,0)) %>% 
  left_join(map_commune) %>% 
  filter(site %in% key_sites) %>% 
  mutate(site_name=paste(zone,"-",site,""))

# Filter max and min values
df_max <- df_anual %>% group_by(site) %>% slice(which.max(year))
df_min <- df_anual %>% group_by(site) %>% slice(which.min(year))

df_anual %>% 
  ggplot(aes(x = year, y = valor)) +
  geom_line(aes(group = site, color = site), size=1.5) +
  geom_point(color = "white", size = 4) +
  geom_point(color = "#0072B2", size = 2) +
  geom_label_repel(data = df_min, 
                   aes(label = site, color=site) , 
                   hjust = "left", 
                   size = 6, 
                   nudge_x = -2.5,
                   direction = "y")+
  geom_label_repel(data = df_max, 
                   aes(label = site, color=site),
                   hjust = "right", 
                   size = 6, 
                   nudge_x = 2.5, 
                   direction = "y")+
  geom_label_repel(aes(label = valor), 
             size = 6, 
             alpha=0.5,
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0)+
  # scale_color_viridis_d()+
  facet_grid(zone~.)+
  scale_y_continuous(name =expression(paste(
    "Annual avg. PM2.5 [",mu,"g/",m^3,"]",sep="")))+
  expand_limits(x=c(2010-2,2019+2))+
  scale_x_continuous(name="", breaks=2008:2021, 
                     labels = c("","",2010:2019,"",""))+
  theme_bw(20)+theme(panel.grid.major = element_blank(),
                     legend.position = "none")
  # geom_hline(yintercept = 20, col="red", linetype = "dashed", size=1)+
  # geom_text(x=2010,y=20, vjust=-1, label="Annual National Standard", col="red")

ggsave(sprintf(file_name,"TimeSeries_Year"), last_plot(),dpi=600,
       width = 16, height = 12, units = "in")
