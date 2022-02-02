
#* Kernel density distribution of profit:

#------ rank design by gwr profit ------#
design_level <- gdata %>%
    .[model == "GWR", ] %>%
    .[, .(profit = mean(profit)), by=c("field_col", "design", "model")] %>%
    .[order(-profit), ] %>%
    .$design
gdata <- gdata[, design := factor(design, levels=design_level)] %>% 
    .[, model := factor(model, levels = c("GWR", "BRF"))]

#------ stack plot ------#
ggplot() +
    stat_density(data = gdata, 
                 aes(x = profit, colour = design, linetype = design),
                 position="identity", geom="line", size=0.5) +
    facet_grid(model~., scales = "free_y") +
    xlim(xlow, xhigh) +
    xlab('Profit Relative to True Optimal ($/ha)') +
    ylab("Density") +
    labs(colour="Design", linetype="Design") +
    theme_classic() +
    theme(
        legend.position='right',
        legend.title = element_text(size=12),
        legend.key.width = unit(1,"cm"),
        legend.text = element_text(size=10),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
        axis.text=element_text(color='black')
    )
