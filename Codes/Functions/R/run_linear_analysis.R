# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_linear_analysis <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels) {
  data <- copy(reg_data)

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

  ols_res <- fixest::feols(feols_formula, data = data)
  

  #************************   RMSR   ***********************
  e_hat_train <- copy(data) %>% 
      .[, y_hat := predict(ols_res, newdata = .)] %>% 
      .[, e_hat := yield - y_hat] %>% 
      .[, .(aunit_id, e_hat)] %>%
      setnames("e_hat", "e_hat_train") 
  e_hat_cv <- copy(cv_data$data[[1]]) %>% 
      .[, y_hat := predict(ols_res, newdata = .)] %>% 
      .[, e_hat := yield - y_hat] %>% 
      .[, .(aunit_id, e_hat)] %>%
      setnames("e_hat", "e_hat_cv") 
  #*********************************************************
 

  N_data <-
    data.table(
      N = seq(min(N_levels), max(N_levels), length = 50)
    ) %>%
    .[, N2 := N^2]

  ols_results <-
    copy(cv_data$data[[1]]) %>%
    .[, `:=`(
      N = NULL,
      N2 = NULL
    )] %>%
    #--- prediction data ---
    expand_grid_df(., N_data) %>%
    .[, y_hat := predict(ols_res, newdata = .)] %>%
    .[, pi_hat := pCorn * y_hat - pN * N] %>%
    #--- optimal N ---
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    #--- RMSE ---
    .[e_hat_train, on = "aunit_id"] %>% 
    .[e_hat_cv, on = "aunit_id"] %>% 
    .[, sim := cv_data$sim]

  return(ols_results)
}