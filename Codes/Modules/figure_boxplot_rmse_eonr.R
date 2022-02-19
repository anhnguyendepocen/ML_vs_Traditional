


#------ range of values ------#
value_ls <- seq(gdata[,min(rmse_eonr)/10] %>% floor()*10, 
                gdata[,max(rmse_eonr)/10] %>% ceiling()*10, 
                by = 10)
yaxis_min <- gdata$rmse_eonr %>% min()
yaxis_max <- gdata$rmse_eonr %>% max()
podg <- position_dodge(0.6)


#------ boxplot ------#
g_plot <- 
    ggplot(data = gdata, 
           aes(x = field_size, y = rmse_eonr, fill = model)) +
    stat_boxplot(geom = "errorbar", width = 0.3, position = podg) +
    geom_boxplot(position = podg, width = 0.5, outlier.shape = NA) +
    ylab('Out-of-Sample EONR Prediction RMSE (kg/ha)') +
    xlab('') +
    scale_y_continuous(breaks = value_ls, label = value_ls) +
    coord_cartesian(ylim = c(yaxis_min, yaxis_max - 10)) +
    theme_bw() +
    theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position='bottom',
        legend.title = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        # legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        # legend.margin=margin(t = -0.5, unit='cm'),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )

