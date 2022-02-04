
#' Prepare data set for sharing with other researchers

rm(list = ls())

# /*===========================================================
#' # Preparation
# /*===========================================================

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
    readRDS(here("Shared/Data/field_data.rds")) 

#* Read field parameters
field_parameters <- 
    readRDS(here("Shared/Data/field_parameters.rds"))

#* Read regression data
field_with_design <- 
    readRDS(here("Shared/Data/field_with_design.rds"))



# /*===========================================================
#' # Price data (global variables)
# /*===========================================================

pCorn = 0.246063    # $/kg
pN = 2.204624       # $/kg

saveRDS(pCorn, here("Shared/Data/ToShare/pCorn.rds"))
saveRDS(pN, here("Shared/Data/ToShare/pN.rds"))



# /*===========================================================
#' # Field map data (full field)
# /*===========================================================

field_sf <- 
    field_data %>% 
    filter(field_col==144) %>% 
    pull(field_sf) %>% 
    .[[1]] %>% 
    data.table(.) %>% 
    .[, col_id := NULL] %>%
    .[, row_id := NULL] %>%
    #---rename lat and lon---
    setnames(
        c("X","Y"),
        c("lon","lat")
    ) %>% 
    st_as_sf()
saveRDS(field_sf, here("Shared/Data/ToShare/field_sf.rds"))



# /*===========================================================
#' # Simulated regression data
# /*===========================================================

#* true cell-level data
field_pars <-
    field_parameters %>% 
    filter(field_col==144) %>% 
    pull(field_pars) %>% 
    .[[1]] %>%
    #---join aunit_id---
    .[data.table(field_sf)[,.(cell_id, aunit_id, lat, lon)], on = "cell_id"] %>% 
    #---true EONR---
    .[, EONR := (pN / pCorn - b1) / (2 * b2)] %>%
    .[, EONR := pmin(Nk, EONR)] %>%
    .[, EONR := pmax(0, EONR)] %>% 
    .[, c("sim", "cell_id", "lon", "lat", "EONR", "aunit_id")] %>% 
    .[order(sim, cell_id), ]

#* aggregate EONR to aunit level
EONR_df <- field_pars %>% 
    .[, .(EONR = mean(EONR)), by = .(sim, aunit_id)]

#* aunit-level regression data
x_vars <-
    c(
        "b0_1", "b0_2",
        "Nk_2_1", "Nk_2_2", "Nk_1_1",
        "plateau_2_1", "plateau_2_2", "plateau_1_1",
        "theta_plateau_1", "theta_plateau_2",
        "theta_Nk_1", "theta_Nk_2",
        "theta_b0_1", "theta_b0_2"
    )
sim_data <-
    field_with_design %>% 
    filter(field_col==144) %>% 
    pull(data_file_name) %>%
    readRDS() %>%
    pull(reg_data) %>%
    .[[1]] %>%
    .[, N_levels := NULL] %>%
    unnest() %>%
    data.table() %>%
    .[, c("sim", "aunit_id", "X", "Y", "N", "yield",  x_vars), with = FALSE] %>%
    setnames(
        names(.),
        c("sim", "aunit_id", "X", "Y", "N", "yield",
          paste0("par", 1:(length(names(.)) - 6))
        )
    ) %>% 
    #---join true EONR---
    EONR_df[., on = c("sim", "aunit_id")]

saveRDS(sim_data, here("Shared/Data/ToShare/sim_data.rds"))



# /*===========================================================
#' # Read me file
# /*===========================================================

ReadMe <- rbind(
    "*** field_sf: spatial map of simulated field (sf)",
    "lon: longitude (meter)",
    "lat: latitude (meter)",
    "cell_id: ID for cell",
    "aunit_id: ID for unit of analysis (2 x 3 cells)",
    "",
    "*** sim_data: simulated experimental data for model estimation (data.table)",
    "sim: simulation ID",
    "aunit_id: ID for unit of analysis (2 x 3 cells)",
    "EONR: true economically optimal N rate",
    "X: aunit-level longitude (meter)",
    "Y: aunit-level latitude (meter)",
    "N: applied N trial rate (kg/ha)",
    "yield: realized yield (kg/ha)",
    "par1 - par14: observed field parameters 1 - 14",
    "",
    "pCorn: corn price ($/kg)",
    "pN: N fertilzier price ($/kg)"
) %>% 
    data.table %>% 
    print()
write.table(ReadMe, here("Shared/Data/ToShare/ReadMe.txt"),
            row.names = FALSE, col.names = FALSE, quote = FALSE)


# /*===========================================================
#' # Visually checking
# /*===========================================================

left_join(field_sf, 
          field_pars[sim==3,], 
          by = c("cell_id","lon", "lat", "aunit_id")) %>% 
    ggplot(data = .) +
    geom_sf(aes(fill = EONR), size = 0.1) +
    scale_fill_gradientn(colours = rev(terrain.colors(100))) +
    theme_void()
ggsave(here("Shared/Data/ToShare/example_field.png"),
       height=4, width=7.2)
