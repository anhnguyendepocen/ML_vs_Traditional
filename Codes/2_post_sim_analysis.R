# /*===========================================================
#' # Preparation
# /*===========================================================

# === Packages ===#
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

#* Set working directory
setwd(here())

#* Load functions
fs::dir_ls(here("Codes", "Functions", "R"), full.names = TRUE) %>%
  lapply(., function(x) source(x))

# /*===========================================================
#' # Load simulated data
# /*===========================================================

#* load generated parameters
field_data <- readRDS(here("Data/field_data.rds"))
field_with_design <- readRDS(here("Data/field_with_design.rds"))
field_parameters <- readRDS(here("Data/field_parameters.rds"))

#* cell-level true data (sprange=600 scenario)
cell_data <- field_parameters$field_pars[[1]]
field_dt <- field_parameters$field_sf[[1]] %>% data.table()
cell_data <- cell_data[field_dt[, .(cell_id, aunit_id)], on = "cell_id"]

#* load simulation results data
mc_sim_results <- readRDS(file = here("Results/mc_sim_results.rds"))

#* aunit-level estimated data
est_data <-
  mc_sim_results$sim_results[[1]] %>%
  unnest(mc_results) %>%
  data.table()

#* price scenarios
pCorn <- mc_sim_results$pCorn
pN <- mc_sim_results$pN

#******************   combine separately simulated data     ********************
{
    #* simulation results
    est_result_ls <-
        readRDS(here("Shared", "Results", "est_result_ls_200.rds"))
    
    for(i in c(3:10)*100){
        est_ls_i <-
            readRDS(here("Shared", "Results", paste0("est_result_ls_", i,".rds")))
        
        #* combine data
        for(sc_i in 1:length(est_result_ls)){
            
            for(m in 1:nrow(est_result_ls[[sc_i]])){
                
                est_result_ls[[sc_i]]$data[[m]] <- 
                    rbind(est_result_ls[[sc_i]]$data[[m]],
                          est_ls_i[[sc_i]]$data[[m]])
                
                est_result_ls[[sc_i]]$perform[[m]] <- 
                    rbind(est_result_ls[[sc_i]]$perform[[m]],
                          est_ls_i[[sc_i]]$perform[[m]])
                
                est_result_ls[[sc_i]] <- 
                    est_result_ls[[sc_i]] %>% 
                    mutate(
                        mean_profit =
                            perform[, mean(profit, na.rm = TRUE)],
                        rmse_train = 
                            perform[, mean(rmse_train, na.rm = TRUE)],
                        rmse_cv = 
                            perform[, mean(rmse_cv, na.rm = TRUE)],
                        rmse_eonr = 
                            perform[, mean(rmse_eonr, na.rm = TRUE)]
                )
            }
        }
    }

    saveRDS(est_result_ls, here("Shared", "Results", "est_result_ls.rds"))
    
}

#*****************************   add EONR data     *****************************
{
    #* simulation results
    est_result_ls <-
        readRDS(here("Shared/Results/est_result_ls_400.rds"))
    
    #* Read field data
    field_data <-
        readRDS(here("Shared/Data/field_data.rds")) 
    
    #* Read field parameters
    field_parameters <- readRDS(here("Shared/Data/field_parameters.rds"))
    
    
    #* for scenario i
    for(sc_i in 1:length(est_result_ls)){
        
        #* prices
        pN <- field_data$pN[sc_i]
        pCorn <- field_data$pCorn[sc_i]
        #* field sf data
        field_sf <- field_data$field_sf[[sc_i]]
        #* field true parameters
        field_pars <- field_parameters$field_pars[[sc_i]] %>% 
            #--- True cell-level EONR ---#
            .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
            .[, opt_N := pmin(Nk, opt_N)] %>%
            .[, opt_N := pmax(0, opt_N)]
        
        #* aggregate EONR to aunit level
        EONR_df <- field_pars %>% 
            #---join aunit_id---
            .[data.table(field_sf)[,.(cell_id, aunit_id)], on = "cell_id"] %>% 
            #---aggregate ---
            .[, .(EONR = mean(opt_N)), by = .(sim, aunit_id)]
        
        #* add EONR data to est_result_ls
        est_result_ls[[sc_i]] <- 
            est_result_ls[[sc_i]] %>% 
            mutate(
                data = list(
                    left_join(data, EONR_df, by = c("sim", "aunit_id"))
                )
            ) %>% 
            mutate(
                perform = list(
                    data.table(data)[, .(rmse_eonr = mean((opt_N_hat - EONR)^2, 
                                                          na.rm = TRUE) %>% sqrt()), 
                                     by = sim] %>% 
                        left_join(perform, ., by = c("sim"))
                )
            ) %>% 
            mutate(
                rmse_eonr = 
                    perform[, mean(rmse_eonr, na.rm = TRUE)]
            )
    }
    
    saveRDS(est_result_ls, here("Shared", "Results", "est_result_ls.rds"))  
}
#******************************************************************************


# /*===========================================================
#' # Profitability Calculation
# /*===========================================================

# -----------------
# Merge data
# -----------------
# === merge true parameters (cell) + estimation (aunit)
merge_data <-
  cell_data[est_data,
    on = c("sim", "aunit_id"),
    allow.cartesian = TRUE
  ]

# -----------------
# Economic analysis
# -----------------
econ_data <-
  copy(merge_data) %>%
  #--- True optima ---#
  .[, opt_N := (pN / pCorn - b1) / (2 * b2)] %>%
  .[, opt_N := pmin(Nk, opt_N)] %>%
  .[, opt_N := pmax(0, opt_N)] %>%
  .[, yield_opt := gen_yield_QP(b0, b1, b2, Nk, opt_N)] %>%
  .[, pi_opt := pCorn * yield_opt - pN * opt_N] %>%
  # :::: based on true response parameters
  #--- SCAM ---#
  .[, yield_scam := gen_yield_QP(b0, b1, b2, Nk, opt_N_scam)] %>%
  .[, pi_scam := pCorn * yield_scam - pN * opt_N_scam] %>%
  .[, pi_scam := pi_scam - pi_opt] %>%
  #--- GWR ---#
  .[, yield_gwr := gen_yield_QP(b0, b1, b2, Nk, opt_N_gwr)] %>%
  .[, pi_gwr := pCorn * yield_gwr - pN * opt_N_gwr] %>%
  .[, pi_gwr := pi_gwr - pi_opt] %>%
  # :::: based on estimated gwr response parameters
  # (i.e., using the estimated quadratic b0_hat, b1_hat, b2_hat
  # as the true response function. Now the pi_scam_est is treated
  # as the pi_opt.)
  #--- SCAM ---#
  .[, yield_scam_est := gen_yield_QD(b0_hat, b1_hat, b2_hat, opt_N_scam)] %>%
  .[, pi_scam_est := pCorn * yield_scam_est - pN * opt_N_scam] %>%
  #--- GWR ---#
  .[, yield_gwr_est := gen_yield_QD(b0_hat, b1_hat, b2_hat, opt_N_gwr)] %>%
  .[, pi_gwr_est := pCorn * yield_gwr_est - pN * opt_N_gwr] %>%
  # :::: GWR gain over SCAM
  #--- profit gain of gwr ---#
  .[, pi_diff := pi_gwr - pi_scam] %>%
  .[, pi_diff_est := pi_gwr_est - pi_scam_est]

# -------------------
# Field level profit
# -------------------
# take field average
pi_data <-
  econ_data %>%
  .[,
    lapply(.SD, mean),
    by = .(sim, transfer),
    .SDcols = c("pi_diff", "pi_diff_est", "opt_N_scam")
  ] %>%
  print()

saveRDS(pi_data, here("Results/pi_data.rds"))

# /*===========================================================
#' # Find an illustrative sim case for overestimation
# /*===========================================================
sim_id <-
  pi_data %>%
  .[transfer == 1, ] %>%
  .[, over_est := pi_diff_est - pi_diff] %>%
  .[, o_diff := abs(over_est - 60)] %>%
  .[which.min(o_diff), sim]

il_data_oe <-
  econ_data[sim == sim_id, ] %>%
  .[transfer == 1, ]

saveRDS(il_data_oe, here("Results/il_data_oe.rds"))

# /*===========================================================
#' # Find extreme cases of simulation results
# /*===========================================================

# ---------------------
# aunit level results
# ---------------------
aunit_results <-
  econ_data[, .(
    sim, aunit_id, b0, b1, b2, Nk, opt_N,
    b0_hat, b1_hat, b2_hat, opt_N_gwr,
    transfer, pi_gwr, pi_gwr_est,
    pi_diff, pi_diff_est
  )] %>%
  .[, lapply(.SD, mean), by = .(sim, transfer, aunit_id)] %>%
  print()

# ------------------------------------------------
# find a simulation with "bad" GWR performances
# ------------------------------------------------

# === look at the percentag of positive N2 coefficient estimates
sim_id <-
  aunit_results %>%
  .[, .(positive_count = sum(b2_hat > 0)), by = sim] %>%
  .[order(-positive_count), ] %>%
  .[1, sim]

# === sim=530 seems to have the worst predicted N results ===#
aunit_sim_single <- aunit_results[sim == sim_id, ]

saveRDS(aunit_sim_single, here("Results/aunit_sim_single.rds"))

# === single simulation results graphing ===#
{
  plot_data <-
    aunit_sim_single %>%
    .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")] %>%
    .[type == "GWR-R", ] %>%
    .[, .(aunit_id, b1, b2, b1_hat, b2_hat)] %>%
    melt(id.var = "aunit_id") %>%
    .[, type := ifelse(str_detect(variable, "hat"), "Estimated", "True")] %>%
    .[, variable := gsub("_hat", "", variable)] %>%
    dcast(aunit_id + variable ~ type, value.var = "value")

  plot_data[variable == "b1", ] %>%
    ggplot(data = .) +
    geom_point(aes(y = Estimated, x = True), size = 0.4) +
    geom_abline(interept = 0, slope = 1, color = "red") +
    xlim(0, NA) +
    ylim(0, NA) +
    coord_equal()

  plot_data[variable == "b2", ] %>%
    ggplot(data = .) +
    geom_point(aes(y = Estimated, x = True), size = 0.4) +
    geom_abline(interept = 0, slope = 1, color = "red") +
    xlim(NA, 0.1) +
    ylim(NA, 0.1) +
    coord_equal()

  aunit_sim_single %>%
    .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")] %>%
    .[type == "GWR-R", ] %>%
    .[, .(aunit_id, opt_N, opt_N_gwr)] %>%
    ggplot(data = .) +
    geom_point(aes(y = opt_N_gwr, x = opt_N), size = 0.4) +
    geom_abline(slope = 1, color = "red") +
    xlab("True Optimal Nitrogen Rate (kg/ha)") +
    ylab("Estimated Optimal Nitrogen Rate (kg/ha)")
}



# -------------------------------------
# extremely over-estimated GWR profits
# -------------------------------------

# === simulations with extreme overestimation cases ===#
extr_ids <- pi_data[pi_diff_est > 400, sim]
extr_data <- econ_data[sim %in% extr_ids, ] %>%
  .[transfer == 1, ] %>%
  .[, case := "extremely overestimated cases"]

# === simulations with normal cases ===#
norm_ids <- pi_data[pi_diff_est < 25 & transfer == 1, sim] %>%
  sample(., size = length(extr_ids), replace = FALSE)
normal_data <- econ_data[sim %in% norm_ids, ] %>%
  .[transfer == 1, ] %>%
  .[, case := "normal cases"]

# === compare extreme and normal cases ===#
rbind(extr_data, normal_data) %>%
  ggplot(
    data = .,
    aes(b1_hat, fill = factor(sim), color = factor(sim))
  ) +
  geom_density(alpha = 0.1) +
  facet_wrap(~case, ncol = 2, scales = "free") +
  ylab("density") +
  xlab("b1_hat by transferred models") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5),
    axis.text = element_text(color = "black")
  )

#* The over-estimated cases are associated with higher b1_hat estimates
rbind(extr_data, normal_data) %>%
  .[, .(
    opt_N_scam = mean(opt_N_scam),
    opt_N_gwr = mean(opt_N_gwr),
    b1_hat = mean(b1_hat),
    b2_hat = mean(b2_hat),
    pi_diff_est = mean(pi_diff_est)
  ),
  by = .(sim, case)
  ] %>%
  print()


# === the case with the most extreme over-estimation ===#
df <- econ_data[sim == 795 & transfer == 1, ] %>%
  .[transfer == 1, ]
df[, b1_hat] %>% summary()
df[, b2_hat] %>% unique()
df[, b0_hat] %>% summary()
df[, opt_N_scam] %>% unique()
df[, opt_N_gwr] %>% summary()
df %>%
  .[, yield_gwr := gen_yield_QP(b0, b1, b2, Nk, 268)] %>%
  .[, yield_gwr_est := gen_yield_QD(b0_hat, b1_hat, b2_hat, 268)] %>%
  ggplot(data = ., aes(x = opt_N_gwr, y = yield_gwr_est)) +
  geom_point()

# === the reason for extreme over-estimation:
# ===   super high or low opt_N_scam estimates, and
# ===   the assumed quadratic GWR response functional form
pi_data %>%
  .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")] %>%
  ggplot(data = ., aes(x = opt_N_scam, y = pi_diff_est, group = type)) +
  geom_point() +
  facet_wrap(~type, ncol = 2) +
  xlab("Estimated optimal N rate by SCAM") +
  ylab("GWR benefit, estimated")