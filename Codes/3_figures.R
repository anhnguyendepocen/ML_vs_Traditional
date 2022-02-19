

#'   Drawing graphs of simulation results
#'

rm(list = ls())

# /*===========================================================
#' # Preparation
# /*===========================================================

#* packages
library(sf)
library(raster)
library(ggplot2)
library(data.table)
library(magrittr)
library(viridis)
library(dplyr)
library(tidyr)
library(tidyverse)
library(modelsummary)
library(tictoc)
library(here)
theme_set(theme_bw())

#* set working directory
setwd(here())

#* source all the R functions in the Functions/R folder
fs::dir_ls(here("GitControlled", "Codes", "Functions", "R"), full.names = TRUE) %>%
    purrr::map(~ source(.))


#' /*=========================================================================*/
#' /*                         Load and Prepare Data                           */
#' /*=========================================================================*/

# -----------------
# load results data
# -----------------
est_data_ls <-
    readRDS(here("Shared/Results/est_result_ls_eonr_100.rds")) %>% 
    print()

# ----------------------
# simulation level data
# ----------------------
est_data <- est_data_ls %>%
    rbindlist() %>% 
    dplyr::select(model, perform, field_col) %>% 
    unnest(perform) %>% 
    data.table() %>% 
    .[, .(field_col, model, sim, profit, rmse_train, rmse_cv, rmse_eonr)] %>% 
    #---field size---
    .[, field_size := paste0( round(field_col * 72 * 6^2 / 10000, 1), " ha")] %>%
    .[, field_size := factor(field_size, levels = c("9.3 ha", "18.7 ha", "37.3 ha"))] %>% 
    #---model name---
    .[model=="brf", model := "BRF"] %>%
    .[model=="rf", model := "RF"] %>%
    .[model=="rf_perfect", model := "RF_b"] %>%
    .[model=="lm", model := "OLS"] %>%
    .[model=="ser_50", model := "SER"] %>%
    .[, model := factor(model, levels = c("RF", "RF_b", "BRF", "OLS", "SER"))] %>% 
    print()



#' /*=========================================================================*/
#' /*                           Mean and Error Bars                           */
#' /*=========================================================================*/

# ----------------
# boxplot: profit
# ----------------
gdata <- est_data
mean_data <- gdata %>%
    .[, .(profit = mean(profit),
          profit_sd = sd(profit),
          IQR = quantile(profit, 0.75) - quantile(profit, 0.25),
          profit_down = quantile(profit, 0.25),
          profit_up = quantile(profit, 0.75)
    ), 
    by=c("field_size", "model")] %>%
    .[, profit_low := profit_down - 1.5*IQR] %>% 
    .[, profit_high := profit_up + 1.5*IQR] %>% 
    print()
source(here("GitControlled/Codes/Modules/figure_boxplot_profit.R"))
ggsave(file = here('GitControlled/Graphs/profits_boxplot.png'),
       height=6,width=6.5)

# ----------------
# boxplot: yield RMSE
# ----------------
gdata <- est_data
source(here("GitControlled/Codes/Modules/figure_boxplot_rmse_yield.R"))
ggsave(file = here('GitControlled/Graphs/rmse_yield_boxplot.png'),
       height=6,width=6.5)

# ----------------
# boxplot: EONR RMSE
# ----------------
gdata <- est_data
source(here("GitControlled/Codes/Modules/figure_boxplot_rmse_eonr.R"))
ggsave(file = here('GitControlled/Graphs/rmse_eonr_boxplot.png'),
       height=6,width=6.5)

# --------------------
# Table: mean and sd 
# --------------------
gdata <- est_data
datasummary(
    (rmse_cv + rmse_eonr + profit) * model ~ factor(field_size) * (Mean + SD), 
    data = gdata[, .(field_size, model, profit, rmse_cv, rmse_eonr)])

mean_data <- gdata %>%
    .[, .(profit = mean(profit),
          profit_sd = sd(profit),
          rmse_yield = mean(rmse_cv),
          rmse_yield_sd = sd(rmse_cv)
    ), 
    by=c("field_size", "model")] %>% 
    .[order(field_size, model), ] %>% 
    print()

# --------------------
# Search simulation round that is closest to the mean results
# --------------------
search_df <- mean_data[, .(field_size, model, rmse_yield)] %>% 
    setnames("rmse_yield", "rmse_yield_mean") %>% 
    .[est_data[, .(field_size, model, sim, rmse_cv)], on = c("field_size", "model")] %>% 
    .[, rmse_diff := abs(rmse_cv - rmse_yield_mean)] %>% 
    #--- smallest diff to mean of all 5 models ---
    .[, rmse_rmse := mean(rmse_diff^2, na.rm = TRUE) %>% sqrt(), by = .(field_size, sim)] %>% 
    .[order(field_size, rmse_rmse, model), ] %>% 
    # .[ , .SD[rmse_rmse==min(rmse_rmse)], by = .(field_size, model)] %>% 
    print()
sim_i_large <- search_df[field_size=="37.3 ha", .(sim = mean(sim))] %>% 
    as.numeric() %>% 
    print()


# ----------------
# combined boxplot
# ----------------
gdata_long <- est_data %>% 
    .[, .(field_size, model, sim, rmse_cv, rmse_eonr, profit)] %>% 
    #=== Wide to Long: melt()
    melt(id.vars = c('field_size','model','sim')) %>% 
    #--- label field size ---#
    .[field_size=="9.3 ha", fsize := "small"] %>% 
    .[field_size=="18.7 ha", fsize := "medium"] %>% 
    .[field_size=="37.3 ha", fsize := "large"] %>% 
    #--- label variable type ---#
    .[variable=="rmse_cv", var_type := "Yield RMSE (kg/ha)"] %>% 
    .[variable=="rmse_eonr", var_type := "EONR RMSE (kg/ha)"] %>% 
    .[variable=="profit", var_type := "Profit ($/ha)"] %>% 
    .[, var_type := factor(var_type, levels = c("Yield RMSE (kg/ha)", 
                                                "EONR RMSE (kg/ha)", 
                                                "Profit ($/ha)"))] %>% 
    print()
for(s in unique(gdata_long$fsize)){
    source(here("GitControlled/Codes/Modules/figure_boxplot_combined.R"))
    ggsave(file = here("GitControlled", "Graphs", 
                       paste0("combined_boxplot_", s, ".png")),
           height=6,width=6.5)
}



#' /*=========================================================================*/
#' /*                       Kernel Density Distribution                       */
#' /*=========================================================================*/

# ---------------
# kernel density
# ---------------
gdata <- est_data
source(here("GitControlled/Codes/Modules/figure_kernel_dist.R"))
ggsave(file = here('GitControlled/Graphs/profits_kernel.png'),
       height=6.5,width=6.5)





#' /*=========================================================================*/
#' /*                         Illustrative Explanation                        */
#' /*=========================================================================*/

# ----------
# load data
# ----------

#* Read field parameters
field_parameters <- readRDS(here("Shared/Data/field_parameters.rds"))

#* Read the simulated data
field_with_design <- readRDS(here("Shared/Data/field_with_design.rds"))

#* Read field data
field_data <-
    readRDS(here("Shared/Data/field_data.rds")) 

#* simulation results
est_data_ls <-
    readRDS(here("Shared/Results/est_result_ls_eonr_100.rds")) %>% 
    print()


#====== full field scenario ===#
sc_i <- 3

#* field sf data
field_sf <- field_data$field_sf[[sc_i]]

#* field true parameters
field_pars <- field_parameters$field_pars[[sc_i]] %>% 
    .[, .(sim, cell_id, b1, b2, Nk)] %>% 
    print()

#* simulated data
sim_data <-
    field_with_design$data_file_name %>% .[sc_i] %>% 
    readRDS()

#* prices
pN <- sim_data$pN
pCorn <- sim_data$pCorn

#* reg data
reg_data <- sim_data$reg_data[[1]] %>% 
    dplyr::select(sim, data) %>% 
    unnest(data) %>% 
    data.table() %>% 
    .[, .(sim, aunit_id, N, yield)] %>% 
    print()

#* results data
pre_data <- est_data_ls[[sc_i]] %>% 
    dplyr::select(model, data, field_col) %>% 
    unnest(data) %>% 
    data.table() %>% 
    #---field size---
    .[, field_size := paste0( round(field_col * 72 * 6^2 / 10000, 1), " ha")] %>%
    .[, field_size := factor(field_size, levels = c("9.3 ha", "18.7 ha", "37.3 ha"))] %>% 
    #---model name---
    .[model=="brf", model := "BRF"] %>%
    .[model=="rf", model := "RF"] %>%
    .[model=="rf_perfect", model := "RF_b"] %>%
    .[model=="lm", model := "OLS"] %>%
    .[model=="ser_50", model := "SER"] %>%
    .[, model := factor(model, levels = c("RF", "RF_b", "BRF", "OLS", "SER"))] %>% 
    print()


# -----------------------------------
# aunit level data for all data
# -----------------------------------

#* aunit-level true EONR
EONR_df <- field_pars %>% 
    #--- cell-level EONR ---#
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)] %>% 
    #---merge aunit_id---#
    .[data.table(field_sf)[, .(cell_id, aunit_id)], on = "cell_id"] %>% 
    #--- aggregate EONR to aunit level
    .[, .(EONR = mean(opt_N)), by = .(sim, aunit_id)]

#* merge all aunit-level data
illus_data <- pre_data %>% 
    left_join(., EONR_df, by = c("sim", "aunit_id")) %>% 
    left_join(., reg_data, by = c("sim", "aunit_id")) %>% 
    #--- retrieve the out-of-sample predicted yield ---#
    data.table() %>% 
    .[, yield_hat := yield - e_hat_cv] %>% 
    print()

#* save illustration data (aunit)
saveRDS(illus_data, here("Shared", "Results", "illus_data.rds"))


# -----------------------------------
# single simulation illustration (aunit-level data)
# -----------------------------------
sim_example = 58
gdata <- illus_data[sim==sim_example, ]
mean_dt <- gdata %>% 
    .[, .(
        rmse_yield = mean((yield - yield_hat)^2, na.rm = TRUE) %>% sqrt() %>% round(1),
        rmse_EONR = mean((EONR - opt_N_hat)^2) %>% sqrt() %>% round(1),
        x_yield = quantile(yield, 0.05),
        y_yield = quantile(yield_hat, 0.99),
        x_EONR = quantile(EONR, 0.05),
        y_EONR = quantile(opt_N_hat, 0.99) + 15
    ),
    by = .(model)] %>% 
    print()

#* predicted vs true yield
source(here("GitControlled/Codes/Modules/figure_single_simu_yield.R"))
ggsave(file = here(paste0("GitControlled/Graphs/single_simu_yield.png")),
       height=7.5,width=6.5)

#* predicted vs true EONR
source(here("GitControlled/Codes/Modules/figure_single_simu_EONR.R"))
ggsave(file = here(paste0("GitControlled/Graphs/single_simu_EONR.png")),
       height=7.5,width=6.5)



#





#' /*=========================================================================*/
#' /*                               Field Maps                                */
#' /*=========================================================================*/

#* observed yield
gdata_sf <- field_pars %>%
    .[sim==58, ] %>% 
    #**************************************************************************
    #*** reg_data N rates only have the non-buffer zone***
    #*** use opt_N to substitute for now ***
    #*** figure it out later ***
    .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, opt_N := pmin(Nk, opt_N)] %>%
    .[, opt_N := pmax(0, opt_N)] %>% 
    .[, yield := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
    .[, .(sim, cell_id, yield)] %>% 
    #**************************************************************************
    #---merge to field sf---
    left_join(field_sf, ., by = c("cell_id"))

#* aggregate to aunit level
gdata_sf <- 
    aggregate(gdata_sf, by = list(gdata_sf$aunit_id), FUN = mean)


#* plot yield map
ggplot() +
    geom_sf(data = gdata_sf, aes(fill = yield), size = 0.1) +
    scale_fill_gradientn(colours = c("red", "yellow", "green4")) +
    theme_void() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 24),
        legend.position = "right",
        legend.title = element_text(size = 18, face = "bold"),
        legend.key.size = unit(0.8, "cm"),
        legend.text = element_text(size = 18)
    ) +
    guides(fill = guide_colorbar(barwidth = 1, barheight = 20,
                                 title = "(kg/ha)",
                                 title.position = "top", title.vjust = 4
    ))
ggsave(file = here("GitControlled", "Graphs", "obs_yield.png"),
       height=6,width=14.4)







#************************************************
#* visual checking
#* map of predicted N recommendation
dt <- est_data[sim==2 & model=="SER", ]
f <- left_join(field_sf, dt, by = c("aunit_id"))
ggplot() +
    geom_sf(data = f, aes(fill = opt_N_hat), size = 0.3) +
    scale_fill_viridis_c(name = "N rate (kg/ha)", direction = -1)
#************************************************
#* map of true EONR 
dt <- EONR_df[sim==2, ]
f <- left_join(field_sf, dt, by = c("aunit_id"))
ggplot() +
    geom_sf(data = f, aes(fill = EONR), size = 0.3) +
    scale_fill_viridis_c(name = "N rate (kg/ha)", direction = -1)
#************************************************







#' /*=========================================================================*/
#' /*                              Count of Winners                           */
#' /*=========================================================================*/

#*******************************************************************************
# load estimation results
est_result_ls <-
    readRDS(here("Results/est_result_ls_100.rds")) %>% 
    rbindlist()

dt <- est_result_ls %>% 
    filter(model %in% c("brf", "lm", "rf", "ser_500")) %>%
    mutate(
        model = replace(model, model=="ser_500", "ser")
    ) %>%
    dplyr::select(model, perform, field_col) %>% 
    unnest(perform) %>% 
    data.table()

perf_dt <- dt %>% 
    .[order(sim, model), ] %>% 
    .[, y := (rmse_cv==min(rmse_cv)) %>% as.numeric(), by = .(sim, field_col)] %>% 
    .[, p := (profit==max(profit)) %>% as.numeric(), by = .(sim, field_col)] %>% 
    .[, yp := y * p] %>% 
    .[, .(n_y = sum(y),
          n_p = sum(p),
          n_yp = sum(yp)), by = .(field_col, model)] %>% 
    print()

perf_dt <- perf_dt %>% 
    dcast(field_col ~ model, value.var = c("n_y", "n_yp")) %>% 
    dplyr::select(field_col, 
                  n_y_rf, n_yp_rf,
                  n_y_brf, n_yp_brf,
                  n_y_lm, n_yp_lm,
                  n_y_ser, n_yp_ser) %>% 
    print()

write.csv(perf_dt, here("Results/perf_dt_500.csv"))
#*******************************************************************************
#*





`