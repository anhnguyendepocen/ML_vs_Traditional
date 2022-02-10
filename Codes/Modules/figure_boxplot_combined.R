


#------ boxplot ------#
podg <- position_dodge(0.6)
ggplot(data = gdata, 
       aes(x = model, y = value, fill = model)) +
    stat_boxplot(geom = "errorbar", width = 0.4, position = podg, lwd = 0.25) +
    geom_boxplot(position = podg, width = 0.4, outlier.shape = NA, lwd = 0.25) +
    # stat_summary(fun = mean, color = "darkred", position = podg,
    #              geom = "point", aes(shape = "Mean"), size = 3) +
    facet_wrap(~variable, ncol = 3, scales = "free_y") +
    ylab('') +
    xlab('') +
    theme_classic() +
    theme(
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position='none',
        legend.title = element_blank(),
        legend.key.size = unit(0.4, 'cm'),
        # legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        # legend.margin=margin(t = -0.5, unit='cm'),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )

