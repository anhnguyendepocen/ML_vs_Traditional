
## Packages

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
fs::dir_ls(here("Codes", "Functions", "R"), full.names = TRUE) %>%
    purrr::map(~ source(.))


# /*===========================================================
#' # Read data
# /*===========================================================

#* Read field data
field_data <-
    readRDS(here("Data/field_data.rds")) %>%
    pull(field_sf) 

#* Read field parameters
field_parameters <- readRDS(here("Data/field_parameters.rds"))

#* Read field data
field_with_design <- readRDS(here("Data/field_with_design.rds"))


#* missing simulations
profit_ls <- readRDS(here("Results", "profit_ls.rds"))
sim_id <- profit_ls[[3]]$data[[1]]$sim %>% unique()
c(501:600)[!501:600 %in% sim_id]




## ---------------------------------
## Define regression functional form 
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
##    data of field size scenario
## ------------------------------

sc_i <- 1

#* field sf data
field_sf <- field_data[[sc_i]]

#* number of simulation cases
nsim <- field_parameters$field_pars[[sc_i]][, max(sim)]

#* load the simulated data
sim_data <-
    field_with_design$data_file_name %>% .[sc_i] %>% 
    readRDS()


#************************************************
#* visual checking
dt <- sim_data$reg_data[[1]][sim == 1, ]$data[[1]]
f <- left_join(field_sf, dt, by = c("aunit_id"))
ggplot() +
    geom_sf(data = f, aes(fill = factor(N_tgt)), size = 0.3) +
    scale_fill_viridis_d(name = "N rate (kg/ha)", direction = -1)
#************************************************


#* spatial weights matrix
#************************************************
# this part should be added to the data preparation codes, and saved in field_data
dt <- sim_data$reg_data[[1]][sim == 1, ]$data[[1]]
D <- matrix(NA, nrow(dt), nrow(dt))             # distance matrix
for (i in 1:nrow(dt)){
    for(j in 1:nrow(dt)){
        D[i,j] <- sqrt((dt$X[i] - dt$X[j])^2+
                           (dt$Y[i] - dt$Y[j])^2 )
    }
}
W <- 1/D^2             # inverse distance weights matrix
diag(W) <- 0
W <- W/rowSums(W)			    # row-standardize
Wls <- mat2listw(W)	            # "listw" object
#*************************************************

#* prices
pN <- sim_data$pN
pCorn <- sim_data$pCorn


#* find true EONR
field_pars <-
    field_parameters$field_pars %>%
    rbindlist() %>%
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)] %>% 
    #--- True optimal profit ---#
    .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
    .[, profit_opt := pCorn * yield_opt - pN * opt_N]



## -----------------------------------------
##  estimation analysis for simulation i
## -----------------------------------------


sim_i <- 4

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

#*#************************** run models ***************************#
ols_results <- run_linear_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels)
ser_results <- run_sperror_analysis(reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls)
#******************************************************************************






#* select the estimation models you want
models_data <-
    data.table(
        model = c(
            "ser", # spatial error
            "lm", # linear quadratic
            "gwr_t", # gwr-trevisan
            "gwr_semi", # gwr-semiparametric
            "gwr_zone_scam", # scam by zone,
            "ma_cf", # multiarm-CF
            "brf", # boosted RF
            "rf", # random forest
            "dmlof_semi", # semiparmetric DML-OF
            "dmlof_quad", # semiparmetric DML-OF
            "drof" # DR-OF
        ),
        on = c(
            TRUE, # ser
            TRUE, # lm
            FALSE, # gwr_t
            FALSE, # gwr_semi
            FALSE, # gwr_zone_scam
            TRUE, # ma_cf
            TRUE, # brf
            FALSE, # rf
            FALSE, # dmlof_semi
            FALSE, # dmlof_quad
            FALSE # drof
        )
    )

# result table
results <-
    models_data %>%
    .[on == TRUE, ] %>%
    rowwise() %>%
    mutate(results = list(
        ols_results.raw
    )) %>%
    mutate(results = list(
        mutate(results, model = model)
    )) %>%
    pull(results) %>%
    rbindlist()


#* calculate profits
est_result <-
    results %>%
    nest_by(model) %>%
    mutate(
        field_col = field_with_design$field_col[sc_i],
        rmse_train = data.table(data)[, mean(e_hat_train^2, na.rm = TRUE) %>% sqrt()],
        rmse_cv = data.table(data)[, mean(e_hat_cv^2, na.rm = TRUE) %>% sqrt()],
        profit =
            field_pars[sim == sim_i+1, ] %>%
            data.table(field_sf)[, .(cell_id, aunit_id)][., on = "cell_id"] %>%
            data.table(data)[., , on = c("sim", "aunit_id")] %>%
            .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N_hat)] %>%
            .[, profit := pCorn * yield - pN * opt_N_hat] %>%
            .[, profit := profit - profit_opt] %>% 
            .[, mean(profit, na.rm = TRUE)]
    )



# data <- copy(reg_data)
#     N_mean <- mean(data$N); N2_mean <- mean(data$N2)
#     N_sd <- sd(data$N); N2_sd <- sd(data$N2)
# test_data <- copy(cv_data$data[[1]])
# 
# 
# # === standardize variables ===#
# data <- data[, (x_vars) := lapply(.SD, scale), .SDcols = x_vars] %>% 
#     .[, N := scale(N)] %>%
#     .[, N2 := scale(N2)]
# test_data <- test_data[, (x_vars) := lapply(.SD, scale), .SDcols = x_vars] %>% 
#     .[, N := scale(N)] %>%
#     .[, N2 := scale(N2)]
# 
# 
# # === spatial error model ===#
# ser_res <- spatialreg::errorsarlm(feols_formula, data = data, listw = Wls)
# 
# # predict optimal N rates
# N_data <-
#     data.table(
#         N = seq(min(N_levels), max(N_levels), length = 50)
#     ) %>%
#     .[, N2 := N^2] %>% 
#     #=== standardize testing N rates ===#
#     .[, N := (N - N_mean) / N_sd] %>%
#     .[, N2 := (N2 - N2_mean) / N2_sd]
# 
# ser_results <-
#     test_data %>%
#     .[, `:=`(
#         N = NULL,
#         N2 = NULL
#     )] %>%
#     expand_grid_df(., N_data) %>%
#     .[, y_hat := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
#     .[, pi_hat := pCorn * y_hat - pN * (N * N_sd + N_mean)] %>%
#     .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
#     .[, .(aunit_id, N)] %>%
#     setnames("N", "opt_N_hat") %>%
#     .[, sim := cv_data$sim]

#************************** *********************** ***************************#
# data <- copy(reg_data)
# test_data <- copy(cv_data$data[[1]])
# 
# 
# # === standardize variables ===#
# data <- data[, (x_vars) := lapply(.SD, scale), .SDcols = x_vars] 
# test_data <- test_data[, (x_vars) := lapply(.SD, scale), .SDcols = x_vars] 
# 
# 
# # === spatial error model ===#
# ser_res <- spatialreg::errorsarlm(feols_formula, data = data, listw = Wls)
# 
# # predict optimal N rates
# N_data <-
#     data.table(
#         N = seq(min(N_levels), max(N_levels), length = 50)
#     ) %>%
#     .[, N2 := N^2] 
# 
# ser_results_0 <-
#     test_data %>%
#     .[, `:=`(
#         N = NULL,
#         N2 = NULL
#     )] %>%
#     expand_grid_df(., N_data) %>%
#     .[, y_hat := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
#     .[, pi_hat := pCorn * y_hat - pN * (N)] %>%
#     .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
#     .[, .(aunit_id, N)] %>%
#     setnames("N", "opt_N_hat") %>%
#     .[, sim := cv_data$sim]

#*******************************************************************************


























