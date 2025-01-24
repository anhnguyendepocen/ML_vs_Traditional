
#' Yield response model estimations

rm(list = ls())

# /*===========================================================
#' # Preparation
# /*===========================================================
#* set up python environment
library(reticulate)

#* packages
library(sp)
library(spdep)
library(spatialreg)
library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gstat)
library(GWmodel)
library(scam)
library(mgcv)
library(magic)
library(stringr)
library(ggplot2)
library(tictoc)
library(here)

#* set working directory
setwd(here())

#* source all the R functions in the Functions/R folder
fs::dir_ls(here("GitControlled", "Codes", "Functions", "R"), full.names = TRUE) %>%
    purrr::map(~ source(.))



# /*===========================================================
#' # Read data
# /*===========================================================

#* Read field data
field_data <-
    readRDS(here("Shared/Data/field_data.rds")) %>%
    pull(field_sf) 

#* Read field parameters
field_parameters <- readRDS(here("Shared/Data/field_parameters.rds"))

#* Read field data
field_with_design <- readRDS(here("Shared/Data/field_with_design.rds"))



# /*===========================================================
#' #  preparations
# /*===========================================================

## ---------------------------------
## Regression functional form 
## ---------------------------------

x_vars <-
    c(
        "b0_1", "b0_2",
        # "Nk_2_1", "Nk_2_2", "Nk_1_1",
        "Nk_2_1", "Nk_2_2", "Nk_1_1", "Nk_1_2",
        # "plateau_2_1", "plateau_2_2", "plateau_1_1",
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


## -----------------------
## Models to run 
## -----------------------

#* select the estimation models you want
models_data <-
    data.table(
        model = c(
            "ser_50", # spatial error, weights_50
            "ser_100", # spatial error, weights_100
            "ser_200", # spatial error, weights_200
            "ser_500", # spatial error, weights_500
            "lm", # linear quadratic
            "gwr_t", # gwr-trevisan
            "gwr_semi", # gwr-semiparametric
            "gwr_zone_scam", # scam by zone,
            "ma_cf", # multiarm-CF
            "brf", # boosted RF
            "rf", # random forest
            "rf_perfect", # random forest with perfect reg data
            "dmlof_semi", # semiparmetric DML-OF
            "dmlof_quad", # semiparmetric DML-OF
            "drof" # DR-OF
        ),
        on = c(
            TRUE, # ser_50
            FALSE, # ser_100
            FALSE, # ser_200
            FALSE, # ser_500
            TRUE, # lm
            FALSE, # gwr_t
            FALSE, # gwr_semi
            FALSE, # gwr_zone_scam
            FALSE, # ma_cf
            TRUE, # brf
            TRUE, # rf
            TRUE, # rf_perfect
            FALSE, # dmlof_semi
            FALSE, # dmlof_quad
            FALSE # drof
        )
    )


## -----------------------------------------
## Estimation function for individual sim_i
## -----------------------------------------

run_analysis <- function(sim_i, x_vars, models_data, pN, pCorn, Wls, nsim) {
    print(sim_i)
    
    # /*+++++++++++++++++++++++++++++++++++
    #' ## Prepare data
    # /*+++++++++++++++++++++++++++++++++++
    # /*+++++++++++++++++++++++++++++++++++
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
    
    # reg_data[, N] %>% hist(breaks = 30)
    # ggplot(reg_data) +
    #   geom_point(aes(y = yield, x = N))
    
    results <-
        models_data %>%
        .[on == TRUE, ] %>%
        rowwise() %>%
        mutate(results = list(
            run_indiv_analysis(model, reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls)
        )) %>%
        mutate(results = list(
            mutate(results, model = model)
        )) %>%
        pull(results) %>%
        rbindlist()
    
    return(results)
}



# /*===========================================================
#' # Run estimations
# /*===========================================================

#****************************************
# need to update the looping codes later
#****************************************

est_result_ls <- list()

#*********************** loop over scenarios *********************#
tic()
for(sc_i in 1:nrow(field_with_design)){

    ## -----------
    ##    data  
    ## -----------
    
    #* field sf data
    field_sf <- field_data[[sc_i]]
    
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
        #--- True cell-level EONR ---#
        .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
        .[, opt_N := pmin(Nk, opt_N)] %>%
        .[, opt_N := pmax(0, opt_N)] %>% 
        #--- True optimal profit ---#
        .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
        .[, profit_opt := pCorn * yield_opt - pN * opt_N]

    
    ## -----------------------
    ## Run estimation models  
    ## -----------------------
    
    #* how many simulations to run
    sim_range <- c(1:100)
    
    #* run pre-defined run_analysis function
    eonr_results <-
        lapply(
            sim_range,
            function(x) 
                tryCatch(
                    run_analysis(sim_i = x, x_vars, models_data, pN, pCorn, Wls, nsim),
                    error = function(e) NULL
                ) 
        ) %>%
        rbindlist()

    #* calculate model performances
    est_result <-
        eonr_results %>%
        nest_by(model) %>%
        mutate(
            perform = list(
                field_pars[sim %in% (sim_range + 1), ] %>%
                    data.table(field_sf)[, .(cell_id, aunit_id)][., on = "cell_id"] %>%
                    data.table(data)[., , on = c("sim", "aunit_id")] %>%
                    .[, y_hat := gen_yield_QP(b0, b1, b2, Nk, opt_N_hat)] %>%
                    .[, profit := pCorn * y_hat - pN * opt_N_hat] %>%
                    .[, profit := profit - profit_opt] %>% 
                    .[, .(profit = mean(profit, na.rm = TRUE),
                          rmse_train = mean(e_hat_train^2, na.rm = TRUE) %>% sqrt(),
                          rmse_cv = mean(e_hat_cv^2, na.rm = TRUE) %>% sqrt(),
                          rmse_eonr = mean((opt_N_hat - opt_N)^2, 
                                           na.rm = TRUE) %>% sqrt()
                          ),
                      by = sim]
                #>>> note: the rmse_eonr is calculated at the cell level (opt_N),
                #>>>      which is larger than the aunit level calculation.
            )
        ) %>% 
        mutate(
            field_col = 
                field_with_design$field_col[sc_i],
            mean_profit =
                perform[, mean(profit, na.rm = TRUE)],
            rmse_train = 
                perform[, mean(rmse_train, na.rm = TRUE)],
            rmse_cv = 
                perform[, mean(rmse_cv, na.rm = TRUE)],
            rmse_eonr = 
                perform[, mean(rmse_eonr, na.rm = TRUE)]
        )
    
    #* profit for scenario i
    est_result_ls[[sc_i]] <- est_result
    
}
toc()
#*********************** end loop over scenarios *********************#


#* save results
saveRDS(est_result_ls, here("Shared", "Results", "est_result_ls_100.rds"))


