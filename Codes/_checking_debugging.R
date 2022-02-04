
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


## ------------------------------
##    data of field size scenario
## ------------------------------

sc_i <- 3

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
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)] %>% 
    #--- True optimal profit ---#
    .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
    .[, profit_opt := pCorn * yield_opt - pN * opt_N]


#************************************************
#* visual checking
# dt <- sim_data$reg_data[[1]][sim == 1, ]$data[[1]]
# f <- left_join(field_sf, dt, by = c("aunit_id"))
# ggplot() +
#     geom_sf(data = f, aes(fill = factor(N_tgt)), size = 0.3) +
#     scale_fill_viridis_d(name = "N rate (kg/ha)", direction = -1)
#************************************************





# /*===========================================================
#' #  estimation analysis for simulation sim_i
# /*===========================================================

sim_i <- 2

    
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



## ------------------------------
##   run models
## ------------------------------

ols_results <- run_linear_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)
# ser_results <- run_sperror_analysis_50(reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls$Wls_50)
rf_results <- run_rf_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)
rf_perfect_results <- run_rf_perfect_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)
# brf_results <- run_brf_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)


#* calculate model performances
perf_df(ols_results)
perf_df(rf_results)
perf_df(rf_perfect_results)


#****************************************************************************
perf_df <- function(model_result){
    perf_df <- field_pars[sim==sim_i+1] %>%
        data.table(field_sf)[, .(cell_id, aunit_id)][., on = "cell_id"] %>%
        data.table(model_result)[., , on = c("sim", "aunit_id")] %>% 
        .[, y_hat := gen_yield_QP(b0, b1, b2, Nk, opt_N_hat)] %>%
        .[, profit := pCorn * y_hat - pN * opt_N_hat] %>%
        .[, profit := profit - profit_opt] %>% 
        .[, .(profit = mean(profit, na.rm = TRUE),
              rmse_train = mean(e_hat_train^2, na.rm = TRUE) %>% sqrt(),
              rmse_cv = mean(e_hat_cv^2, na.rm = TRUE) %>% sqrt()
        ),
        by = sim]
    return(perf_df)
}  
#****************************************************************************

rf_results <- run_rf_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)
rf_results$opt_N_hat %>% hist(breaks=100)
perf_df_ls[[sim_i]] <- perf_df(rf_results)


        
        
