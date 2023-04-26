### MortalityRR-PM2.5
## Figure 2 showing RR and their 95% CI in the article
## PBH March 2021


# load data
source("Scripts/00-CargaLibrerias.R", encoding = "UTF-8")
df <- read.delim("Data/Data_Model/Model_sensitivity.csv",sep=";")

df <- df %>% 
  mutate(case=str_replace_all(case,"Fixed Radius","Cutoff Distance"),
         case=str_replace_all(case,"Population commune","Population"),
         case=str_replace_all(case,"Only Metropolitan region","Metropolitan region only"),
         case=str_replace_all(case,"10% in each extreme of PM2.5 excluded","PM2.5 within P10-P90"))


levels_causes <- c("All Causes (A00-Q99)","Cardiopulmonary (all I and J)",
                   "Cardiovascular (all I)","Pulmonary (all J)")
levels_case <- c("Base", "Cutoff Distance 50km", "Cutoff Distance 100km",
                 "Population >50,000", 
                 "Metropolitan region only","Without Metropolitan region",
                 "PM2.5 within P10-P90")


# Figure
df %>% 
  rowid_to_column() %>% 
  filter(term=="mp25_10um") %>% 
  # create labels and binary to highlight red points
  mutate(cause=factor(cause,levels_causes),
         case=factor(case,levels_case),
         sign=conf.low>1,
         est_label=round(estimate,2),
         estimate_label=paste0(round(estimate,2),
                               " (",round(p.value,2),")")) %>% 
  # remove models not desired
  filter(!is.na(cause)) %>% 
  # figure
  ggplot(aes(x=fct_rev(case), y=estimate))+
  geom_linerange(aes(ymin=conf.low, ymax=conf.high))+ # bars
  geom_point(aes(col=sign), size=3.5)+ #points
  geom_text(aes(label=est_label),nudge_x = 0.43,nudge_y = 0.03)+ # LABELhttp://127.0.0.1:35119/graphics/plot_zoom_png?width=1200&height=900
  scale_color_manual(values = c("#666666","red"))+guides(col=F)+ # signficant as red
  geom_hline(yintercept = 1, linetype = "dashed")+ # vertical line at 1
  facet_wrap(~cause)+ # by Cause (dependent variable)
  scale_y_continuous(limits = c(0.83,1.72),
                     breaks = seq(0.9, 1.4, by = 0.1),
                     labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  coord_flip(ylim = c(0.9,1.4))+ # set limits to exclude long tails
  labs(x="",
       y=expression(paste(
         "MRR: Excess risk per an increase in 10 ","\u03BCg/m\u00B3"," PM2.5"),sep=""), 
       # caption="MRR with C.I. 95% under different endpoints. Red point indicates a significant effect."
  )+
  theme_bw(16)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))

# Save figure
pdf("Figures/Sensitivity/MMR_summary_nb_label.pdf",
    width = 14.87, height = 9.30)
last_plot()
dev.off()

ggsave(file="Figures/Sensitivity/MMR_summary_nb_label.svg", 
       plot=last_plot(), width=14.87, height=9.30,units = "in")

#EoF