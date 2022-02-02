


#------ range of profit values ------#
value_ls <- -seq(0, -gdata[,min(profit)/5] %>% ceiling()*5, by = 10)
yaxis_min <- gdata$profit %>% min()
yaxis_max <- gdata$profit %>% max()

#------ boxplot ------#
library(tidytext)
podg <- position_dodge(0.5)
ggplot(data = gdata, 
       aes(x = model, y = profit)) +
    stat_boxplot(geom = "errorbar", width = 0.25, position = podg) +
    geom_boxplot(position = podg, width = 0.25, outlier.shape = NA) +
    geom_text(data = mean_data, 
              aes(x = model, y = profit_high, 
                  label = paste0(round(profit, 2), " (", round(profit_sd, 2),")")),
              position = podg, angle = 90, 
              hjust = 0, size = 3 ) +
    facet_wrap(~field_size, nrow = 1, strip.position = "top", scales = "free") +
    ylab('Average Profit Relative to True Optimal ($/ha)') +
    xlab('') +
    scale_x_reordered(position = "bottom") +
    scale_y_continuous(expand = c(0, 0), breaks = value_ls, label = value_ls,
                       limits = c(yaxis_min, yaxis_max + 10)) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position='none',
        legend.title = element_text(size=12),
        legend.key.size = unit(0.4, 'cm'),
        legend.text = element_text(size=10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    ) 

 
