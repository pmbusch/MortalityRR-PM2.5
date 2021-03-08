### MortalityRR-PM2.5
## MR Pyramid
## PBH Feb 2021

# load(".RData")
source("Scripts/00-Functions.R", encoding = "UTF-8")
theme_set(theme_bw(22)+theme(panel.grid.major = element_blank()))

# Load  Data ----------------
source("Scripts/Load_Data/deis_load.R", encoding = "UTF-8")
source("Scripts/Aggregate_Data/population_agg.R", encoding = "UTF-8")

## Calculate MR rates -----------
df_population_deis %>% names()
df_deis %>% names()

# Get for causes
df_deis_diseases <- df_deis %>% 
  mutate(cause=case_when(
    categ_diag1 %in% c("C34","C33") ~"LCA",
    str_detect(subcateg_diag1,"C") ~ "CAN",
    str_detect(subcateg_diag1,"J") ~ "RSP",
    str_detect(subcateg_diag1,"I") ~ "CVD",
    is.na(cap_diag2) ~ "All Causes",
    !is.na(cap_diag2) ~ "Ext. Causes")) %>% 
  mutate(sex=if_else(sexo=="Hombre","Male","Female")) %>% 
  group_by(cause,sex, age_group) %>% 
  summarize(deaths=n()/3) %>% ungroup() #average for 3 years

#Pop
df_pop <- df_population_deis %>%
  mutate(sex=ifelse(sexo=="hombre","Male","Female")) %>% 
  group_by(sex, age_group) %>% 
  summarize(pob=sum(poblacion)) %>% ungroup()
df_pop$pob %>% sum()

# Join with population
df_mr_fig <- df_deis_diseases %>% 
  left_join(df_pop) %>% 
  mutate(mr=deaths/pob*1e5,
         age_group=factor(age_group,
                          levels=c("0-4","5-9","10-14","15-19","20-24","25-29",
                                   "30-34","35-39","40-44","45-49","50-54","55-59",
                                   "60-64","65-69","70-74","75-79","80+")))

  
# Combine causes
df_long <- df_mr_fig %>% select(-deaths,-pob) %>% 
  spread(cause,mr) %>% 
  replace_na(list(LCA=0))

df_long <- df_long %>% 
  mutate(`All Causes`=`All Causes`+CVD+RSP+CAN+LCA,
         CDP=CVD+RSP,
         CAN=CAN+LCA)

df_mr_fig <- df_long %>% gather(cause, mr, -sex,-age_group)

# Change sign and do a log transformation
df_mr_fig <- df_mr_fig %>% 
  mutate(mr=if_else(mr>1,mr,0), # to avoid problems with log trans
         mr_log=if_else(sex=="Female",-log(mr,10),log(mr,10)),
         mr_log=if_else(is.infinite(mr_log),0,mr_log),
         mr=if_else(sex=="Female",-mr,mr))

# A MR below 1 is highly uncertain

## Function to generate pyramid figure ----------
f_mrPyramid <- function(df, 
                        cause_f, 
                        title="",
                        brks_f=seq(-5000, 5000, 1000),
                        log_scale=F,
                        label_f=F){
  
  # Generate break lables
  brks <- brks_f
  if (log_scale){
    brks <- ifelse(brks>=0, log(brks,10), -log(-brks,10))
    brks[(length(brks)+1)/2] <- 0
  }
  
  step_seq <- brks_f[2]-brks_f[1]
  lbls = paste0(as.character(c(seq(-min(brks_f), 0, -step_seq), 
                               seq(step_seq, max(brks_f), step_seq))), "")
  lbls <- lbls %>% str_replace_all("000","K")
  
  caption_f <- if_else(log_scale, 
                       "Source: Census Bureau 2017 and DEIS \n Scale is in log base 10",
                       "Source: Census Bureau 2017 and DEIS")
  
  nudge <- if_else(df$mr>0,0.5,-0.5)
  
  # Plot
  p <- df %>% 
    mutate(mr_label=if_else(mr>0,mr,-mr) %>% round(0),
           log_scale_f=log_scale,
           mr_plot=if_else(log_scale_f,mr_log,mr)) %>% 
    filter(cause==cause_f) %>%
    ggplot(aes(x = age_group, y = mr_plot, fill = sex)) +   # Fill column
    geom_bar(stat = "identity", width = .6) +   # draw the bars
    scale_y_continuous(breaks = brks, # Breaks
                       labels = lbls,) + # Labels
    coord_flip() +  # Flip axes 
    labs(x="Age Group",
         y=title, 
         fill="Gender",
         caption = caption_f) +
    theme(plot.title = element_text(hjust = .5), 
          axis.text.x = element_text(angle = 90, size=12),
          axis.ticks = element_blank(),
          plot.caption = element_text(size=16,face = "italic")) +
    scale_fill_brewer(palette = "Dark2")  # Color palette
  
  if (label_f){
    return(p+geom_text(aes(label=mr_label), size=5, nudge_y=nudge))
  } else {
    return(p)
  }
  
  
}

# Test
f_mrPyramid(df_mr_fig,"CDP",
            title = "Mortality rate Cardiopulmonary (all I and J) [per 100,000]",
            brks_f = seq(-5000, 5000, 1000),
            log_scale = T,
            label_f = T)

## PDF ----------------
# Global parameters
url_pdf <- "Paper_escritura/Supplementary/MortalityRates/%s.pdf"
log_scale_all <- T
label_all <- T
if (log_scale_all & label_all){
  file_name <- "MR_log_label"
} else if(log_scale_all){
  file_name <- "MR_log"
} else if(label_all){
  file_name <- "MR_label"
} else {
  file_name <- "MR"
}

#
# pdf script -----
pdf(sprintf(url_pdf,file_name),
    width = 14.87, height = 9.30)

# All Causes
f_mrPyramid(df_mr_fig,"All Causes",
            title = "Mortality rate All Causes (A00-Q99) [per 100,000]",
            brks_f = seq(-10000, 10000, 2000),
            log_scale = log_scale_all,
            label_f = label_all)

# CDP
f_mrPyramid(df_mr_fig,"CDP",
            title = "Mortality rate Cardiopulmonary (all I and J) [per 100,000]",
            brks_f = seq(-5000, 5000, 1000),
            log_scale = log_scale_all,
            label_f = label_all)

# CVD
f_mrPyramid(df_mr_fig,"CVD",
            title = "Mortality rate Cardiovascular (all I) [per 100,000]",
            brks_f = seq(-3000, 3000, 1000),
            log_scale = log_scale_all,
            label_f = label_all)

# RSP
f_mrPyramid(df_mr_fig,"RSP",
            title = "Mortality rate Pulmonary (all J) [per 100,000]",
            brks_f = seq(-2000, 2000, 1000),
            log_scale = log_scale_all,
            label_f = label_all)

# CAN
f_mrPyramid(df_mr_fig,"CAN",
            title = "Mortality rate Cancer (all C) [per 100,000]",
            brks_f = seq(-2000, 2000, 1000),
            log_scale = log_scale_all,
            label_f = label_all)


# LUNG CAN (LCA)
f_mrPyramid(df_mr_fig,"LCA",
            title = "Mortality rate Lung Cancer (C33-C34) [per 100,000]",
            brks_f = seq(-1000, 1000, 1000),
            log_scale = log_scale_all,
            label_f = label_all)

# Ext. Causes
f_mrPyramid(df_mr_fig,"Ext. Causes",
            title = "Mortality rate External Causes [per 100,000]",
            brks_f = seq(-500, 500, 500),
            log_scale = log_scale_all,
            label_f = label_all)

dev.off()


# Figures Example --------------
# Ref: http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html#Population%20Pyramid
# X Axis Breaks and Labels 



df_mr_fig$mr %>% range()
df_mr_fig$mr_log %>% range()
brks <- seq(-5000, 5000, 1000)
brks <- ifelse(brks>=0, log(brks,10), -log(-brks,10))
brks[6] <- 0
lbls = paste0(as.character(c(seq(5000, 0, -1000), seq(1000, 5000, 1000))), "")
lbls <- lbls %>% str_replace_all("000","K")

nudge <- if_else(df_mr_fig$mr>0,0.5,-0.5)

# Plot
df_mr_fig %>% 
  mutate(mr_label=if_else(mr>0,mr,-mr) %>% round(0)) %>% 
  filter(cause=="RSP") %>%
  ggplot(aes(x = age_group, y = mr_log, fill = sex)) +   # Fill column
  geom_bar(stat = "identity", width = .6) +   # draw the bars
  # facet_wrap(~cause)+
  geom_text(aes(label=mr_label), size=5, nudge_y=nudge)+
  scale_y_continuous(breaks = brks, # Breaks
                     # trans = "log10",
                     labels = lbls,) + # Labels
  coord_flip() +  # Flip axes 
  labs(x="Age Group",
       y="Mortality rate Cardiopulmonary (all I and J) [per 100,000]", 
       fill="Gender",
       caption = "Source: Census Bureau 2017 and DEIS \n Scale is in log base 10") +
  theme(plot.title = element_text(hjust = .5), 
        axis.text.x = element_text(angle = 90, size=12),
        axis.ticks = element_blank(),
        plot.caption = element_text(size=16,face = "italic")) +
  scale_fill_brewer(palette = "Dark2")  # Color palette


# Save figure
f_savePlot(last_plot(), file_path = "Figures/mrCDPPyramid.png")


## EoF