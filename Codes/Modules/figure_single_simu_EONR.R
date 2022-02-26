
#* predicted vs true EONR

ggplot(data = gdata) +
    geom_point(aes(x = EONR, y = opt_N_hat), size = 0.5) +
    geom_text(
        data = mean_dt, hjust = 0, size = 2.5,
        aes(x = x_EONR, y = y_EONR, label = paste0("RMSE=", rmse_EONR))) +
    geom_abline(intercept = 0, slope = 1, color = "red", size = 1) +
    facet_wrap(~ model, ncol = 3) +
    ylab("Out-of-Sample Predicted EONR (kg/ha)") +
    xlab("True EONR (kg/ha)") +
    theme_bw() +
    theme(
        panel.grid = element_blank(),
        legend.position='none',
        legend.title = element_blank(),
        axis.text=element_text(color='black')
    )


