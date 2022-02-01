# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

# reg_data <- scaled_reg_data
# cv_data <- scaled_cv_data

run_rf_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels) {
  data <- copy(reg_data)

  X <-
    data[, c("N", x_vars), with = FALSE] %>%
    data.frame()

  Y <- data[, yield]

  RF_temp <-
    grf::regression_forest(
      X = X,
      Y = Y,
      num.trees = 2000,
      # min.node.size = 10
      tune.parameters = "all"
    )

  #************************   RMSR   ***********************
  e_hat_train <- copy(data) %>%
    .[, y_hat := predict(RF_temp,
      newdata = .[, c("N", x_vars), with = FALSE]
    )] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_train")
  e_hat_cv <- copy(cv_data$data[[1]]) %>%
    .[, y_hat := predict(RF_temp,
      newdata = .[, c("N", x_vars), with = FALSE]
    )] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_cv")
  #*********************************************************

  rf_results <-
    copy(cv_data$data[[1]]) %>%
    .[, yield_hat := predict(RF_temp,
      newdata = .[, c("N", x_vars), with = FALSE]
    )] %>%
    .[, pi_hat := pCorn * yield_hat - pN * N] %>%
    .[, .SD[which.max(pi_hat)], by = aunit_id] %>%
    .[, opt_N_hat := N] %>%
    .[, .(aunit_id, opt_N_hat)] %>%
    #--- RMSE ---
    .[e_hat_train, on = "aunit_id"] %>%
    .[e_hat_cv, on = "aunit_id"] %>%
    .[, sim := cv_data$sim]

  return(rf_results)
}