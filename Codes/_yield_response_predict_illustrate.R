
#' Yield response curve prediction at specific sites


# /*===========================================================
#' #  Data preparations
# /*===========================================================

## ---------------------------------
## Regression functional form 
## ---------------------------------

x_vars <-
    c(
        "b0_1", "b0_2",
        "Nk_2_1", "Nk_2_2", "Nk_1_1", "Nk_1_2",
        "plateau_2_1", "plateau_2_2", "plateau_1_1", "plateau_1_2",
        "theta_plateau_1", "theta_plateau_2",
        "theta_Nk_1", "theta_Nk_2",
        "theta_b0_1", "theta_b0_2"
    )

control_vars <- paste0(x_vars, collapse = "+")
int_vars <-
    paste0(
        paste0("I(N * ", x_vars, ")", collapse = "+"),
        "+",
        paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

feols_formula <-
    paste0(
        "yield ~ N + N2 + ",
        control_vars, "+",
        int_vars
    ) %>%
    formula()


## ------------------------------
## data of field size scenario
## ------------------------------

sc_i <- 3

#* field sf data
field_sf <- field_data$field_sf[[sc_i]]

#* field true parameters
field_pars <- field_parameters$field_pars[[sc_i]]

#* number of simulation cases
nsim <- field_pars[, max(sim)]

#* load the simulated data
sim_data <-
    field_with_design$data_file_name %>% .[sc_i] %>% 
    readRDS()

#* spatial weights matrix
Wls <- field_with_design$weights_matrix[[sc_i]]

#* prices
pN <- sim_data$pN
pCorn <- sim_data$pCorn


## -----------------------
## Find true optimal N  
## -----------------------

#* find true EONR
field_pars <- field_pars %>%
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)] %>% 
    #--- True optimal profit ---#
    .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
    .[, profit_opt := pCorn * yield_opt - pN * opt_N]



# /*===========================================================
#' #  estimation analysis for simulation sim_i
# /*===========================================================

sim_i <- 58

## -----------------------
## reg data
## -----------------------

#* extract the data for the sim_i
train_data <- sim_data$reg_data[[1]][sim == sim_i, ]
cv_data <- sim_data$reg_data[[1]][sim == ifelse(sim_i + 1 > nsim, 1, sim_i + 1), ]

#* define parameters
N_levels <- train_data$N_levels[[1]]
reg_data <-
    train_data$data[[1]] %>%
    .[, x := (X - min(X)) / (max(X) - min(X))] %>%
    .[, y := (Y - min(Y)) / (max(Y) - min(Y))] %>%
    .[, xy := x * y] %>%
    .[, N2 := N^2]



## ------------------------------
## run models
## ------------------------------
{
    tic()
    ols_res <- fixest::feols(feols_formula, data = reg_data)
    
    X <- reg_data[, c("N", x_vars), with = FALSE]
    Y <- reg_data[, yield]
    RF_temp <-
        grf::regression_forest(
            X = X,
            Y = Y,
            num.trees = 2000,
            tune.parameters = "all"
        )
    BRF_temp <-
        grf::boosted_regression_forest(
            X = X,
            Y = Y,
            num.trees = 2000,
            tune.parameters = "all"
        )
    best_vars <- c("Nk", "plateau", "b0")
    X <- reg_data[, c("N", best_vars), with = FALSE]
    Y <- reg_data[, yield]
    RF_b_temp <-
        grf::regression_forest(
            X = X,
            Y = Y,
            num.trees = 2000,
            tune.parameters = "all"
        )
    #**************************
    # ser
    #* Scale the train data
    scaler_data <-
        reg_data[, c("aunit_id", "yield", "N", "N2", x_vars), with = FALSE] %>%
        melt(id.vars = "aunit_id") %>%
        nest_by(variable) %>%
        mutate(scaler = 1 / max(data$value)) %>%
        data.table() %>%
        .[, .(variable, scaler)]
    scaled_train_data <-
        reg_data[, c("aunit_id", "yield", "N", "N2", x_vars), with = FALSE] %>%
        scale_data(., scaler_data)
    
    ser_res <- spatialreg::errorsarlm(feols_formula, data = scaled_train_data, listw = Wls$Wls_50)
    #**************************
    toc()
}



#******************************************************************************
#* Treatment effects: true vs. estimated
{
    N_data <-
        data.table(
            N = N_levels
        ) %>%
        .[, N2 := N^2]
    pred_1 <-
        copy(cv_data$data[[1]]) %>%
        .[, `:=`(
            N = NULL,
            N2 = NULL
        )] %>%
        #--- prediction data ---
        expand_grid_df(., N_data) %>%
        #--- true yield ---
        .[, y := gen_yield_QP(b0, b1, b2, Nk, N)] %>%
        #--- predicted yield ---
        .[, y_hat_lm := predict(ols_res, newdata = .)] %>%
        .[, y_hat_rf := predict(RF_temp, newdata = .[, c("N", x_vars), with = FALSE])] %>%
        .[, y_hat_brf := predict(BRF_temp, newdata = .[, c("N", x_vars), with = FALSE])] %>%
        .[, y_hat_rf_b := predict(RF_b_temp, newdata = .[, c("N", best_vars), with = FALSE])] %>%
        .[, .(aunit_id, N, y, y_hat_lm, y_hat_rf, y_hat_brf, y_hat_rf_b)] %>% 
        .[, N := round(N, 2)]
    pred_2 <-
        copy(cv_data$data[[1]]) %>%
        .[, `:=`(
            N = NULL,
            N2 = NULL
        )] %>%
        #--- prediction data ---
        expand_grid_df(., N_data) %>%
        .[, c("aunit_id", "N", "N2", x_vars), with = FALSE] %>%
        scale_data(., scaler_data) %>%
        #--- predicted yield ---
        .[, yield := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
        .[, .(aunit_id, yield, N)] %>%
        scale_data(., scaler_data, back = TRUE) %>%
        setnames("yield", "y_hat_ser") %>%
        .[, .(aunit_id, N, y_hat_ser)] %>% 
        .[, N := round(N, 2)] %>% 
        .[, sim := sim_i]
    pred_curve <- pred_1[pred_2, on = c("aunit_id", "N")] %>% 
        #--- time lag terms ---#
        .[, N_next := c(N[-1], NA), by = .(sim, aunit_id)] %>% 
        .[, y_next := c(y[-1], NA), by = .(sim, aunit_id)] %>% 
        .[, y_hat_lm_next := c(y_hat_lm[-1], NA), by = .(sim, aunit_id)] %>% 
        .[, y_hat_rf_next := c(y_hat_rf[-1], NA), by = .(sim, aunit_id)] %>% 
        .[, y_hat_rf_b_next := c(y_hat_rf_b[-1], NA), by = .(sim, aunit_id)] %>% 
        .[, y_hat_brf_next := c(y_hat_brf[-1], NA), by = .(sim, aunit_id)] %>% 
        .[, y_hat_ser_next := c(y_hat_ser[-1], NA), by = .(sim, aunit_id)] %>% 
        #--- treatment effects of N ---#
        .[, me_true := (y_next - y)] %>% 
        .[, me_lm := (y_hat_lm_next - y_hat_lm), by = .(sim, aunit_id)] %>% 
        .[, me_rf := (y_hat_rf_next - y_hat_rf), by = .(sim, aunit_id)] %>% 
        .[, me_rf_b := (y_hat_rf_b_next - y_hat_rf_b), by = .(sim, aunit_id)] %>% 
        .[, me_brf := (y_hat_brf_next - y_hat_brf), by = .(sim, aunit_id)] %>% 
        .[, me_ser := (y_hat_ser_next - y_hat_ser), by = .(sim, aunit_id)] %>% 
        print()
    pred_curve[, .(sim, aunit_id, N, me_true, me_rf, me_rf_b, me_brf,
                   me_lm, me_ser)] %>% 
        #=== Wide to Long: melt() ===#
        melt(id.vars = c( "sim", "aunit_id", "N", "me_true")) %>%
        data.table() %>%
        na.omit(.) %>% 
        #=== model name ===#
        .[variable=="me_rf", model := "RF"] %>%
        .[variable=="me_rf_b", model := "RF_b"] %>%
        .[variable=="me_brf", model := "BRF"] %>%
        .[variable=="me_lm", model := "LM"] %>%
        .[variable=="me_ser", model := "SER"] %>%
        .[, model := factor(model, levels = c("RF", "RF_b", "BRF", "LM", "SER"))] %>% 
        #=== treatment name ===#
        .[N==N_levels[1], trt := paste0(N_levels[1], " to ", N_levels[2], " kg N/ha")] %>%
        .[N==N_levels[2], trt := paste0(N_levels[2], " to ", N_levels[3], " kg N/ha")] %>%
        .[N==N_levels[3], trt := paste0(N_levels[3], " to ", N_levels[4], " kg N/ha")] %>%
        .[N==N_levels[4], trt := paste0(N_levels[4], " to ", N_levels[5], " kg N/ha")] %>%
        #=== graphing ===#
        ggplot(data = .) +
        geom_point(aes(x = me_true, y = value), size = 0.2) +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        facet_grid(trt ~ model) +
        ylab("Estimated Treatment Effect (kg/ha yield)") +
        xlab("True Treatment Effect (kg/ha yield)") +
        theme_bw() +
        theme(
            panel.grid = element_blank(),
            axis.text = element_text(color='black')
        )
    ggsave(file = here(paste0('GitControlled/Graphs/treatment_sim_', sim_i,'.png')),
           height=7.5,width=6.5)
}




#*******************************************************************************
#* prediction of response curve: RMSE of marginal effects, kernel
{
    N_data <-
        data.table(
            N = seq(min(N_levels), max(N_levels), length = 50)
        ) %>%
        .[, N2 := N^2]
    pred_1 <-
        copy(cv_data$data[[1]]) %>%
        .[, `:=`(
            N = NULL,
            N2 = NULL
        )] %>%
        #--- prediction data ---
        expand_grid_df(., N_data) %>%
        #--- true yield ---
        .[, y := gen_yield_QP(b0, b1, b2, Nk, N)] %>%
        #--- predicted yield ---
        .[, y_hat_lm := predict(ols_res, newdata = .)] %>%
        .[, y_hat_rf := predict(RF_temp, newdata = .[, c("N", x_vars), with = FALSE])] %>%
        .[, y_hat_brf := predict(BRF_temp, newdata = .[, c("N", x_vars), with = FALSE])] %>%
        .[, y_hat_rf_b := predict(RF_b_temp, newdata = .[, c("N", best_vars), with = FALSE])] %>%
        .[, .(aunit_id, N, y, y_hat_lm, y_hat_rf, y_hat_brf, y_hat_rf_b)] %>% 
        .[, N := round(N, 2)]
    pred_2 <-
        copy(cv_data$data[[1]]) %>%
        .[, `:=`(
            N = NULL,
            N2 = NULL
        )] %>%
        #--- prediction data ---
        expand_grid_df(., N_data) %>%
        .[, c("aunit_id", "N", "N2", x_vars), with = FALSE] %>%
        scale_data(., scaler_data) %>%
        #--- predicted yield ---
        .[, yield := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
        .[, .(aunit_id, yield, N)] %>%
        scale_data(., scaler_data, back = TRUE) %>%
        setnames("yield", "y_hat_ser") %>%
        .[, .(aunit_id, N, y_hat_ser)] %>% 
        .[, N := round(N, 2)] %>% 
        .[, sim := sim_i]
    pred_curve <- pred_1[pred_2, on = c("aunit_id", "N")] %>% 
        #--- time lag terms ---#
        .[, N_lag := c(NA, N[-.N]), by = .(sim, aunit_id)] %>% 
        .[, y_lag := c(NA, y[-.N]), by = .(sim, aunit_id)] %>% 
        .[, y_hat_lm_lag := c(NA, y_hat_lm[-.N]), by = .(sim, aunit_id)] %>% 
        .[, y_hat_rf_lag := c(NA, y_hat_rf[-.N]), by = .(sim, aunit_id)] %>% 
        .[, y_hat_brf_lag := c(NA, y_hat_brf[-.N]), by = .(sim, aunit_id)] %>% 
        .[, y_hat_ser_lag := c(NA, y_hat_ser[-.N]), by = .(sim, aunit_id)] %>% 
        #--- marginal effects of N ---#
        .[, me_true := (y - y_lag)/(N - N_lag)] %>% 
        .[, me_lm := (y_hat_lm - y_hat_lm_lag)/(N - N_lag), by = .(sim, aunit_id)] %>% 
        .[, me_rf := (y_hat_rf - y_hat_rf_lag)/(N - N_lag), by = .(sim, aunit_id)] %>% 
        .[, me_brf := (y_hat_brf - y_hat_brf_lag)/(N - N_lag), by = .(sim, aunit_id)] %>% 
        .[, me_ser := (y_hat_ser - y_hat_ser_lag)/(N - N_lag), by = .(sim, aunit_id)] %>% 
        print()
    #--- RMSE of marginal effect estimations ---#
    rmse_df <- pred_curve[, .(
        rmse_me_lm = mean((me_lm - me_true)^2, na.rm = TRUE) %>% sqrt(),
        rmse_me_rf = mean((me_rf - me_true)^2, na.rm = TRUE) %>% sqrt(),
        rmse_me_brf = mean((me_brf - me_true)^2, na.rm = TRUE) %>% sqrt(),
        rmse_me_ser = mean((me_ser - me_true)^2, na.rm = TRUE) %>% sqrt()
    ), 
    by = .(sim, aunit_id)]
    rmse_df[, lapply(.SD, mean), by = sim] %>% print()
    rmse_df %>% 
        #=== Wide to Long: melt() ===#
        melt(id.vars = c( "sim", "aunit_id")) %>%
        data.table() %>%
        #=== model name ===#
        .[variable=="rmse_me_rf", model := "RF"] %>%
        .[variable=="rmse_me_brf", model := "BRF"] %>%
        .[variable=="rmse_me_lm", model := "LM"] %>%
        .[variable=="rmse_me_ser", model := "SER"] %>%
        .[, model := factor(model, levels = c("RF", "BRF", "LM", "SER"))] %>% 
        #=== graphing ===#
        ggplot(data = .) +
        stat_density(aes(x = value, colour = model, linetype = model),
                     position="identity", geom="line", size=1) +
        xlab('RMSE of Marginal Effects') +
        ylab("Density") +
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
}



#*******************************************************************************

#* yield curves
for(aunit in 802){
    pred_curve[aunit_id==aunit, .(sim, aunit_id, N, y, y_hat_rf, y_hat_rf_b, y_hat_brf,
                                  y_hat_lm, y_hat_ser)] %>% 
        #=== Wide to Long: melt() ===#
        melt(id.vars = c( "sim", "aunit_id", "N")) %>%
        data.table() %>%
        #=== model name ===#
        .[variable=="y", model := "True"] %>%
        .[variable=="y_hat_rf", model := "RF"] %>%
        .[variable=="y_hat_rf_b", model := "RF_b"] %>%
        .[variable=="y_hat_brf", model := "BRF"] %>%
        .[variable=="y_hat_lm", model := "LM"] %>%
        .[variable=="y_hat_ser", model := "SER"] %>%
        .[, model := factor(model, levels = c("True", "RF", "RF_b", "BRF", "LM", "SER"))] %>% 
        #=== graphing ===#
        ggplot(data = .) +
        geom_line(aes(x = N, y = value, color = model, size = model)) +
        geom_point(data = cv_data$data[[1]][aunit_id==aunit, ], 
                  aes(x = N, y = gen_yield_QP(b0, b1, b2, Nk, N)), 
                  color = "black", shape = 1, size = 4, stroke = 2) +
        ylab("yield (kg/ha)") +
        scale_size_manual(values = c(2, 1, 1, 1, 1, 1)) + 
        scale_colour_manual(values = c("black", "red", "orange", "purple", "green", "blue"))
    ggsave(file = here(paste0('GitControlled/Graphs/curves/', aunit,'_yield.png')),
           height=6.5,width=6.5)
    # pred_curve[aunit_id==aunit, .(sim, aunit_id, N, me_true, me_rf, me_brf,
    #                               me_lm, me_ser)] %>% 
    #     #=== Wide to Long: melt() ===#
    #     melt(id.vars = c( "sim", "aunit_id", "N")) %>%
    #     data.table() %>%
    #     #=== model name ===#
    #     .[variable=="me_true", model := "True"] %>%
    #     .[variable=="me_rf", model := "RF"] %>%
    #     .[variable=="me_brf", model := "BRF"] %>%
    #     .[variable=="me_lm", model := "LM"] %>%
    #     .[variable=="me_ser", model := "SER"] %>%
    #     .[, model := factor(model, levels = c("True", "RF", "BRF", "LM", "SER"))] %>% 
    #     #=== graphing ===#
    #     ggplot(data = .) +
    #     geom_line(aes(x = N, y = value, color = model, size = model)) +
    #     ylab("marginal effect of N") +
    #     scale_size_manual(values = c(2, 1, 1, 1, 1)) + 
    #     scale_colour_manual(values = c("black", "red", "purple", "green", "blue"))
    # ggsave(file = here(paste0('GitControlled/Graphs/curves/', aunit,'_marginal.png')),
    #        height=6.5,width=6.5)
}


# sim 2, aunit_id 504
# sim 3, aunit_id 320
# sim 7, aunit_id 47, 99, 114, 115, 118, 152
# sim 58, aunit_id 212, 283, 284, 802







        
        
