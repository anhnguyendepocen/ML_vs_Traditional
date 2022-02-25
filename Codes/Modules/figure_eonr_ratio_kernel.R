
#* Average ratio of estimated EONR to true EONR
#* Kernel density distribution

g_plot <- gdata %>%
    #------ EONR_hat/EONR ratio for each aunit ------#
    .[, ratio := opt_N_hat / EONR] %>% 
    #------ average ratio for each simulation ------#
    .[, .(ratio = mean(ratio)), by=c("field_size", "sim", "model")] %>%
    #------ kernel density plot ------#
    ggplot(data = .) +
    geom_density(aes(x = ratio, color = model, fill = model),
                 position = "identity", geom = "line", size = 0.5,
                 alpha = 0.35) +
    # coord_cartesian(xlim = c(0.6, 1.5)) +
    xlim(0.6, 1.5) +
    xlab('Average Ratio of Estimated EONR to True EONR') +
    ylab("Density") +
    labs(color = "Model", fill = "Model") +
    theme_bw() +
    theme(
        legend.position='bottom',
        legend.title = element_blank(),
        legend.key.width = unit(0.75, 'cm'),
        legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    ) +
    guides(fill = guide_legend(label.position = "right",
                               keywidth = unit(0.5, "cm"),
                               keyheight = unit(0.5, "cm"),
                               title.position = "left", 
                               title.vjust = 1)) 
