


#------ boxplot ------#
podg <- position_dodge(0.6)
ggplot(data = gdata_long[fsize==s], 
       aes(x = model, y = value, fill = model)) +
    stat_boxplot(geom = "errorbar", width = 0.4, position = podg, lwd = 0.25) +
    geom_boxplot(position = podg, width = 0.4, outlier.shape = NA, lwd = 0.25) +
    facet_wrap(~ var_type, ncol = 3, scales = "free_y") +
    ylab('') +
    xlab('') +
    theme_bw() +
    theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position='none',
        legend.title = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )
