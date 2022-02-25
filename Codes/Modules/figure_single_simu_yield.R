
#* predicted vs true yield

ggplot(data = gdata) +
    geom_point(aes(x = yield, y = yield_hat), size = 0.5) +
    geom_text(
        data = mean_dt, hjust = 0, size = 2.5,
        aes(x = x_yield, y = y_yield, label = paste0("RMSE=", rmse_yield))) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
    facet_wrap(~ model, ncol = 3) +
    ylab("Out-of-Sample Predicted Yield (kg/ha)") +
    xlab("Observed Yield (kg/ha)") +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        legend.position='none',
        legend.title = element_blank(),
        axis.text=element_text(color='black')
    )


