
#* Kernel density distribution of profit:

# #------ rank design by gwr profit ------#
# design_level <- gdata %>%
#     .[model == "GWR", ] %>%
#     .[, .(profit = mean(profit)), by=c("field_col", "design", "model")] %>%
#     .[order(-profit), ] %>%
#     .$design
# gdata <- gdata[, design := factor(design, levels=design_level)] %>% 
#     .[, model := factor(model, levels = c("GWR", "BRF"))]

#------ stack plot ------#
ggplot() +
    stat_density(data = gdata, 
                 aes(x = profit, colour = model, linetype = model),
                 position="identity", geom="line", size=0.5) +
    facet_wrap(~field_size, ncol = 1, strip.position = "top", scales = "free_y") +
    # xlim(xlow, xhigh) +
    xlab('Profit Relative to True Optimal ($/ha)') +
    ylab("Density") +
    # labs(colour="Design", linetype="Design") +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position='bottom',
        legend.title = element_blank(),
        legend.key.width = unit(1, 'cm'),
        legend.text = element_text(margin = margin(r = 1, unit = "cm")),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    ) +
    guides(fill = guide_legend(label.position = "right",
                               keywidth = unit(0.5, "cm"),
                               keyheight = unit(0.5, "cm"),
                               title.position = "left", 
                               title.vjust = 1)) 
