### MortalityRR-PM2.5
## Age Pyramid
## PBH Dec 2020

# load(".RData")
source("Scripts/00-Functions.R", encoding = "UTF-8")
theme_set(theme_bw(22)+theme(panel.grid.major = element_blank()))

# Load population Data ----------------
df_popPyramid <- left_join(censo_2017_comunas, codigos_territoriales)

# Create Age group
df_popPyramid <- df_popPyramid %>% 
  mutate(age_group=case_when(
    edad %in% c("0 a 4") ~ "0-4",
    edad %in% c("5 a 9") ~ "5-9",
    edad %in% c("10 a 14") ~ "10-14",
    edad %in% c("15 a 19") ~ "15-19",
    edad %in% c("20 a 24") ~ "20-24",
    edad %in% c("25 a 29") ~ "25-29",
    edad %in% c("30 a 34") ~ "30-34",
    edad %in% c("35 a 39") ~ "35-39",
    edad %in% c("40 a 44") ~ "40-44",
    edad %in% c("45 a 49") ~ "45-49",
    edad %in% c("50 a 54") ~ "50-54",
    edad %in% c("55 a 59") ~ "55-59",
    edad %in% c("60 a 64") ~ "60-64",
    edad %in% c("65 a 69") ~ "65-69",
    edad %in% c("70 a 74") ~ "70-74",
    edad %in% c("75 a 79") ~ "75-79",
    T ~ "80+") %>% 
      factor(levels=c("0-4","5-9","10-14","15-19","20-24","25-29",
                      "30-34","35-39","40-44","45-49","50-54","55-59",
                      "60-64","65-69","70-74","75-79","80+")))



df_popPyramid$sexo %>% unique()
df_popPyramid <- df_popPyramid %>% 
  mutate(sex=if_else(sexo=="hombre","Male","Female"))

# Summary per age and sex
df_popPyramid <- df_popPyramid %>% 
  group_by(sex, age_group) %>% 
  summarise(pop=sum(poblacion, na.rm = T)) %>% ungroup()
df_popPyramid$pop %>% sum()


# Change sign
df_popPyramid <- df_popPyramid %>% 
  mutate(pop=if_else(sex=="Female",-pop,pop)/1e3)



# Figure --------------
# Ref: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Population%20Pyramid
# X Axis Breaks and Labels 
df_popPyramid$pop %>% range()
brks <- seq(-750, 750, 250)
lbls = paste0(as.character(c(seq(750, 0, -250), seq(250, 750, 250))), "")

# Plot
ggplot(df_popPyramid, aes(x = age_group, y = pop, fill = sex)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  scale_y_continuous(breaks = brks,   # Breaks
                     labels = lbls) + # Labels
  coord_flip() +  # Flip axes 
  labs(title="Age Pyramid Chile",
       x="Age Group",y="Population [thousands]", fill="Gender",
       caption = "Source: Census Bureau 2017") +
  theme(plot.title = element_text(hjust = .5), 
        axis.ticks = element_blank(),
        plot.caption = element_text(size=16,face = "italic")) +
  scale_fill_brewer(palette = "Dark2")  # Color palette

# Save figure
f_savePlot(last_plot(), file_path = "Figures/agePyramid.png")


## EoF