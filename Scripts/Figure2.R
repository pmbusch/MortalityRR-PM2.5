### MortalityRR-PM2.5
## Figure 2 showing RR and their 95% CI in the article
## PBH March 2021


# load data

df <- read.delim("Data/Data_Model/Model_sensitivity.csv",sep=";")

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
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high))+ # bars
  geom_point(aes(col=sign), size=2, alpha=.5)+ #points
  geom_text(aes(label=est_label),nudge_x = 0.3)+ # LABEL
  scale_color_manual(values = c("#666666","red"))+guides(col=F)+ # signficant as red
  geom_hline(yintercept = 1, linetype = "dashed")+ # vertical line at 1
  facet_wrap(~cause)+ # by Cause (dependent variable)
  coord_flip()+
  scale_y_continuous(labels = function(x) format(x, big.mark = " ",scientific = FALSE))+
  labs(x="",
       y=expression(paste(
         "MRR: Excess risk per an increase in 10 ","\u03BCg/m\u00B3"," PM2.5"),sep=""), 
       # caption="MRR with C.I. 95% under different endpoints. Red point indicates a significant effect."
  )+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(size=10, lineheight=.5))

# Save figure
pdf("Figures/Sensitivity/MMR_summary_nb_label.pdf",
    width = 14.87, height = 9.30)
last_plot()
dev.off()

#EoF