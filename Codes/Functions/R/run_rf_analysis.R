# /*===========================================================
#' # RF analysis
# /*===========================================================
# ! Run RF and do economic analysis


run_rf_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels, include_int = FALSE) {
  train_data <- copy(reg_data)
  test_data <- copy(cv_data$data[[1]])

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Define X
  # /*+++++++++++++++++++++++++++++++++++
  if (include_int == FALSE) {
    all_X_vars <- x_vars
  } else {
    int_var_names <- paste0("N_", x_vars)
    int_var_exp <- paste0("= N * ", x_vars)
    int_var_gen_exp <- paste0(int_var_names, int_var_exp, collapse = ",")

    all_X_vars <- c(x_vars, int_var_names)

    eval(parse(text = paste("train_data[, `:=`(", int_var_gen_exp, ")]", sep = "")))
    eval(parse(text = paste("test_data[, `:=`(", int_var_gen_exp, ")]", sep = "")))
  }

  X <- train_data[, c("N", all_X_vars), with = FALSE]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Define Y
  # /*+++++++++++++++++++++++++++++++++++
  Y <- train_data[, yield]

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Run RF
  # /*+++++++++++++++++++++++++++++++++++
  RF_temp <-
    grf::regression_forest(
      X = X,
      Y = Y,
      num.trees = 2000,
      # min.node.size = 10,
      tune.parameters = "all"
    )

  #************************   RMSE   ***********************
  e_hat_train <-
    train_data %>%
    .[, y_hat := predict(RF_temp, newdata = .[, c("N", all_X_vars), with = FALSE])] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_train")

  e_hat_cv <-
    test_data %>%
    .[, y_hat := predict(RF_temp, newdata = .[, c("N", all_X_vars), with = FALSE])] %>%
    .[, e_hat := yield - y_hat] %>%
    .[, .(aunit_id, e_hat)] %>%
    setnames("e_hat", "e_hat_cv")
  #*********************************************************

  N_seq <-
    data.table(
      N = seq(min(train_data$N), max(train_data$N), by = 2)
    )

  rf_results <-
    test_data %>%
    .[, c("aunit_id", "yield", all_X_vars), with = FALSE] %>%
    expand_grid_df(., N_seq) %>%
    .[, yield_hat := predict(RF_temp, newdata = .[, c("N", all_X_vars), with = FALSE])] %>%
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