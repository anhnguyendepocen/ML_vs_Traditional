# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis


run_sperror_analysis_raw <- function(reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls) {
    data <- copy(reg_data)
    test_data <- copy(cv_data$data[[1]])

  control_vars <- paste0(x_vars, collapse = "+")
  int_vars <-
    paste0(
      paste0("I(N * ", x_vars, ")", collapse = "+"),
      "+",
      paste0("I(N2 * ", x_vars, ")", collapse = "+")
    )

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Regression
  # /*+++++++++++++++++++++++++++++++++++
  feols_formula <-
    paste0(
      "yield ~ N + N2 + ",
      control_vars, "+",
      int_vars
    ) %>%
    formula()

  # === spatial error model ===#
  ser_res <- spatialreg::errorsarlm(feols_formula, data = data, listw = Wls)

  # /*+++++++++++++++++++++++++++++++++++
  #' ## Find EONR
  # /*+++++++++++++++++++++++++++++++++++
  #* define N space
  N_data <-
    data.table(
      N = seq(min(N_levels), max(N_levels), length = 50)
    ) %>%
    .[, N2 := N^2]

  ser_results <-
    copy(test_data) %>%
    .[, `:=`(
      N = NULL,
      N2 = NULL
    )] %>%
    expand_grid_df(., N_data) %>%
    .[, c("aunit_id", "N", "N2", x_vars), with = FALSE] %>%
    .[, yield := predict(ser_res, newdata = .) %>% data.frame() %>% pull(trend)] %>%
    .[, .(aunit_id, yield, N)] %>%
    .[, pi_hat := pCorn * yield - pN * N] %>%
    .[, .SD[which.max(pi_hat), ], by = aunit_id] %>%
    .[, .(aunit_id, N)] %>%
    setnames("N", "opt_N_hat") %>%
    .[, sim := cv_data$sim]

  return(ser_results)
}