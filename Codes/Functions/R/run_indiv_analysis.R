# /*===========================================================
#' # GWR analysis
# /*===========================================================
# ! Run GWR and do economic analysis

run_indiv_analysis <- function(model, reg_data, cv_data, x_vars, pN, pCorn, N_levels, Wls) {
  if (model == "lm") { # linear quadratic
    results <-
      run_linear_analysis(
        reg_data,
        cv_data,
        x_vars,
        pN, pCorn,
        N_levels
      )
  } else if (model == "ser_50") { # spatial error
    results <-
      run_sperror_analysis_50(
          reg_data = reg_data,
          cv_data = cv_data,
          x_vars = x_vars,
          pN = pN, 
          pCorn = pCorn, 
          N_levels = N_levels,
          Wls = Wls$Wls_50
      )
  } else if (model == "ser_100") { # spatial error
      results <-
          run_sperror_analysis_100(
              reg_data = reg_data,
              cv_data = cv_data,
              x_vars = x_vars,
              pN = pN, 
              pCorn = pCorn, 
              N_levels = N_levels,
              Wls = Wls$Wls_100
          )
  } else if (model == "ser_200") { # spatial error
      results <-
          run_sperror_analysis_200(
              reg_data = reg_data,
              cv_data = cv_data,
              x_vars = x_vars,
              pN = pN, 
              pCorn = pCorn, 
              N_levels = N_levels,
              Wls = Wls$Wls_200
          )
  } else if (model == "ser_500") { # spatial error
      results <-
          run_sperror_analysis_500(
              reg_data = reg_data,
              cv_data = cv_data,
              x_vars = x_vars,
              pN = pN, 
              pCorn = pCorn, 
              N_levels = N_levels,
              Wls = Wls$Wls_500
          )
  } else if (model == "gwr_t") { # gwr-trevisan
    results <-
      run_GWR_analysis(
        reg_data,
        N_levels,
        pN, pCorn
      )
  } else if (model == "gwr_semi") { # gwr-semiparametric
    results <-
      run_GWR_semi_analysis(
        gam_formula = formula(yield ~ s(N, k = 4), m = 2),
        reg_data,
        N_levels,
        pN, pCorn
      )
  } else if (model == "gwr_zone_scam") { # scam by zone
    gwr_results <-
      run_GWR_analysis(
        reg_data,
        N_levels,
        pN, pCorn
      )
    results <-
      run_gwr_zone_scam_analysis(
        gwr_results = gwr_results,
        reg_results = reg_results,
        num_zones = 3,
        reg_data = reg_data,
        pCorn = pCorn,
        pN = pN
      )
  } else if (model == "ma_cf") { # multiarm-CF
    results <-
      run_macf_analysis(
        reg_data,
        cv_data,
        x_vars,
        pN,
        pCorn,
        N_levels
      )
  } else if (model == "brf") { # brf
    results <-
      run_brf_analysis(
        reg_data,
        cv_data,
        x_vars,
        pN,
        pCorn,
        N_levels
      )
  } else if (model == "rf") { # rf
      results <-
          run_rf_analysis(
              reg_data,
              cv_data,
              x_vars,
              pN,
              pCorn,
              N_levels
          )
  } else if (model == "rf_perfect") { # rf_perfect
      results <-
          run_rf_perfect_analysis(
              reg_data,
              cv_data,
              x_vars,
              pN,
              pCorn,
              N_levels
          )
  } else if (model == "dmlof_semi") { # semiparmetric DML-OF
    results <-
      run_DML_OF_c_analysis(
        gam_formula = formula(yield ~ s(N, k = 4, m = 2)),
        x_vars = x_vars,
        w_vars = c(x_vars, "x", "y"),
        reg_data = reg_data,
        cv_data = cv_data,
        pN = pN,
        pCorn = pCorn,
        N_levels = N_levels
      )
  } else if (model == "drof") { # DR-OF
    results <-
      run_drof_analysis(
        reg_data,
        x_vars,
        pN,
        pCorn,
        N_levels
      )
  }
}