


#------ range of profit values ------#
value_ls <- -seq(0, -gdata[,min(profit)/5] %>% ceiling()*5, by = 10)
yaxis_min <- gdata$profit %>% min()
yaxis_max <- gdata$profit %>% max()

#------ boxplot ------#
library(tidytext)
podg <- position_dodge(0.5)
ggplot(data = gdata, 
       aes(x = field_size, y = profit, fill = model)) +
    stat_boxplot(geom = "errorbar", width = 0.25, position = podg) +
    geom_boxplot(position = podg, width = 0.25, outlier.shape = NA) +
    geom_text(data = mean_data, 
              aes(x = field_size, y = profit_high,
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
              position = podg, angle = 90, 
              hjust = 0, size = 3 ) +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    scale_y_continuous(expand = c(0, 0), breaks = value_ls, label = value_ls,
                       limits = c(yaxis_min, 0)) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position='bottom',
        legend.title = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    ) +
    guides(fill = guide_legend(label.position = "right",
                               keywidth = unit(0.5, "cm"),
                               keyheight = unit(0.5, "cm"),
                               title.position = "left", 
                               title.vjust = 1)) 


