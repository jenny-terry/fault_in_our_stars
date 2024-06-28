library(magrittr)
options(scipen = 999)
options(max.print = 100000)

# read in core data (note that the parsing issues are only to do with data types, which can be fixed as required)
core_data <- here::here("data/core_data.csv")|> readr::read_csv()

# select vars
mod_data <- core_data |>
  dplyr::select(university, stars, rmars)

# set reference level
mod_data$university <- as.factor(mod_data$university)
mod_data <- within(mod_data, university <- relevel(university, ref = "University of Sussex"))

# run it in base (not robust)
mod_lm_base <- mod_data |> lm(stars ~ rmars*university, data = _)
summary(mod_lm_base)

# it drops the 5 unis with very low n, so let's use the data with them excluded
mod_data2 <- mod_data |> dplyr::filter(university != "Avila University" 
                                       & university != "Leeds Trinity University" 
                                       & university != "Menoufia University" 
                                       & university != "National and Kapodistrian University of Athens" 
                                       & university != "University of Ghana")

mod_lm_base2 <- mod_data2 |> lm(stars ~ rmars*university, data = _)
summary(mod_lm_base2)

# get standardised bs the old fashioned way
mod_lm_std <- parameters::model_parameters(mod_lm_base2, standardize = "refit")

# do assumption checks

plot(mod_lm_base, which = c(1, 3))
# resid vs. fitted shows no non-linearity or heteroscedasticity
# scale-location (more sensitive) suggests some minor non-linearity

plot(mod_lm_base, which = 2)
# qq resid plot shows some non-normality, but in the present sample, it isn't a concern

plot(mod_lm_base, which = c(4:6))
# cooks distance all well below 1
# leverage plot's red line looks fine so no violations
# leverage plot's no red dashed lines for high cooks distance values
# leveage/cooks/std resid plot looks weird

mod_lm_base_rsd <- mod_lm_base |> 
  broom::augment() |> 
  tibble::rowid_to_column(var = "case_no") 

mod_lm_base_rsd_196 <- mod_lm_base_rsd |> 
  dplyr::filter(abs(.std.resid) >= 1.96) |>
  dplyr::select(case_no, .std.resid, .resid) |> 
  dplyr::arrange(.std.resid)

sr_percent_196 <- nrow(mod_lm_base_rsd_196)/nrow(mod_data)*100

mod_lm_base_rsd_3 <- mod_lm_base_rsd |> 
  dplyr::filter(abs(.std.resid) >= 3) |>
  dplyr::select(case_no, .std.resid, .resid) |> 
  dplyr::arrange(.std.resid)

sr_percent_3 <- nrow(mod_lm_base_rsd_3)/nrow(mod_data)*100

# get corrs and compare to sample size
coeffs <- mod_lm_std$Coefficient
params <- mod_lm_std$Parameter
se <- mod_lm_std$SE

coeff_tbl <- tibble::tibble(params, coeffs, se, corrs)

####### FOREST PLOT #######

#read in table with corrs from the ss
forest_data <- here::here("tables/mod_lm_std_forest.csv")|> readr::read_csv()

# get forest plot
forest_plot <- forest_data |>
  dplyr::arrange(desc(corr)) |>
  forestplot::forestplot(labeltext = c(university, n),
                         mean = beta,
                         lower = conf.low,
                         upper = conf.high,
                         clip = c(-1, 1),
                         xticks = c(-1, -0.9, -0.8, -0.7, -0.6, -0.5, -0.4, -0.3, 
                                    -0.2, -0.1, 0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
                         xlog = F) |>
  forestplot::fp_append_row(mean  = 0.0,
                            lower = 0,
                            upper = 0,
                            university = "University of Sussex",
                            n = "317",
                            is.summary = FALSE) |>
  forestplot::fp_set_zebra_style("#f9f9f9") |>
  forestplot::fp_set_style(txt_gp = forestplot::fpTxtGp(label = list(gpar(fontfamily = "serif", cex = 0.7)),
                                                        ticks = gpar(fontfamily = "serif", cex = 0.7)))
forest_plot

