library(magrittr)
options(scipen = 999)
options(max.print = 100000)

# read in core data (note that the parsing issues are only to do with data types, which can be fixed as required)
core_data <- here::here("data/core_data.csv")|> readr::read_csv()

# select items
cfa_data <- core_data |> 
  dplyr::select(Q7.1_1:Q7.1_23, Q7.2_1:Q7.2_23, Q8.1_1:Q8.1_20, Q8.2_1:Q8.2_24)

# rename items
cfa_data %<>% 
  dplyr::rename(
    stars_test1 = Q7.1_1, 
    stars_int1 = Q7.1_2, 
    stars_help1 = Q7.1_3, 
    stars_test2 = Q7.1_4, 
    stars_int2 = Q7.1_5, 
    stars_int3 = Q7.1_6, 
    stars_int4 = Q7.1_7, 
    stars_test3 = Q7.1_8, 
    stars_int5 = Q7.1_9, 
    stars_test4 = Q7.1_10, 
    stars_int6 = Q7.1_11, 
    stars_int7 = Q7.1_12, 
    stars_test5 = Q7.1_13, 
    stars_int8 = Q7.1_14, 
    stars_test6 = Q7.1_15, 
    stars_help2 = Q7.1_16,
    stars_int9 = Q7.1_17, 
    stars_int10 = Q7.1_18,
    stars_help3 = Q7.1_19,
    stars_int11 = Q7.1_20, 
    stars_test7 = Q7.1_21, 
    stars_test8 = Q7.1_22,
    stars_help4 = Q7.1_23,
    
    rmars_s_test1 = Q7.2_1, 
    rmars_s_test2 = Q7.2_2, 
    rmars_s_test3 = Q7.2_3, 
    rmars_s_test4 = Q7.2_4, 
    rmars_s_test5 = Q7.2_5, 
    rmars_s_test6 = Q7.2_6, 
    rmars_s_test7 = Q7.2_7, 
    rmars_s_test8 = Q7.2_8, 
    rmars_s_test9 = Q7.2_9, 
    rmars_s_test10 = Q7.2_10, 
    rmars_s_num2 = Q7.2_11, 
    rmars_s_num3 = Q7.2_12, 
    rmars_s_num4 = Q7.2_13, 
    rmars_s_num5 = Q7.2_14,
    rmars_s_course1 = Q7.2_15, 
    rmars_s_course2 = Q7.2_16, 
    rmars_s_course3 = Q7.2_17, 
    rmars_s_course4 = Q7.2_18,
    rmars_s_course5 = Q7.2_19,
    rmars_s_num6 = Q7.2_20, 
    rmars_s_num7 = Q7.2_21, 
    rmars_s_num8 = Q7.2_22, 
    rmars_s_num9 = Q7.2_23,
    
    stars_m_test1 = Q8.1_1, 
    stars_m_int1 = Q8.1_2, 
    stars_m_help1 = Q8.1_3, 
    stars_m_test2 = Q8.1_4, 
    stars_m_int2 = Q8.1_5, 
    stars_m_int3 = Q8.1_6, 
    stars_m_int4 = Q8.1_7, 
    stars_m_test3 = Q8.1_8, 
    stars_m_test4 = Q8.1_9, 
    stars_m_int6 = Q8.1_10, 
    stars_m_test5 = Q8.1_11, 
    stars_m_int8 = Q8.1_12, 
    stars_m_test6 = Q8.1_13, 
    stars_m_help2 = Q8.1_14,
    stars_m_int10 = Q8.1_15,
    stars_m_help3 = Q8.1_16,
    stars_m_int11 = Q8.1_17, 
    stars_m_test7 = Q8.1_18, 
    stars_m_test8 = Q8.1_19,
    stars_m_help4 = Q8.1_20,
    
    rmars_test1 = Q8.2_1, 
    rmars_test2 = Q8.2_2, 
    rmars_test3 = Q8.2_3, 
    rmars_test4 = Q8.2_4, 
    rmars_test5 = Q8.2_5, 
    rmars_test6 = Q8.2_6, 
    rmars_test7 = Q8.2_7, 
    rmars_test8 = Q8.2_8, 
    rmars_test9 = Q8.2_9, 
    rmars_test10 = Q8.2_10, 
    rmars_num1 = Q8.2_11, 
    rmars_num2 = Q8.2_12, 
    rmars_num3 = Q8.2_13, 
    rmars_num4 = Q8.2_14, 
    rmars_num5 = Q8.2_15, 
    rmars_course1 = Q8.2_16, 
    rmars_course2 = Q8.2_17, 
    rmars_course3 = Q8.2_18, 
    rmars_course4 = Q8.2_19,
    rmars_course5 = Q8.2_20,
    rmars_num6 = Q8.2_21, 
    rmars_num7 = Q8.2_22, 
    rmars_num8 = Q8.2_23, 
    rmars_num9 = Q8.2_24
  )

#' Analysis 1: Is the factor structure obtained in the EFA (herein ‘the EFA model’) 
#' a good fit of the data?

# meas_mod_efa8

efa8_pr_mod <- '
int_num =~ stars_int3 + stars_m_int3 + stars_int11 + stars_m_int6 + stars_m_int11 + stars_int1 + 
  stars_int10 + stars_m_int10 + stars_int5 + stars_int2 + stars_m_int1 + stars_m_int2 + 
  stars_int4 + stars_int9 + stars_int7 + stars_m_int8 + stars_int6 + stars_m_int4 + 
  stars_m_test5 + stars_test5 + rmars_s_course4 + rmars_course4 + stars_int8

tests =~ rmars_s_test5 + rmars_s_test6 + rmars_s_test7 + rmars_test5 + rmars_test6 +
  stars_test6 + stars_test1 + rmars_test7 + rmars_s_test1 + stars_m_test6 +
  stars_test3 + stars_test4 + rmars_test1 + rmars_s_test4 + rmars_s_test3 +
  stars_m_test1 + stars_m_test4 + stars_m_test3 + rmars_test3 + rmars_test4

calc_basic =~ rmars_s_num6 + rmars_s_num8 + rmars_num3 + rmars_s_num7 + rmars_s_num9 +
  rmars_num4 + rmars_num5 + rmars_num2 + rmars_s_num4 + rmars_s_num2 +
  rmars_s_num5 + rmars_s_num3

help =~ stars_help1 + stars_m_help1 + stars_m_help2 + stars_help2 + 
  stars_help3 + stars_m_help3 + stars_help4 + stars_m_help4

course =~ rmars_course3 + stars_m_test7 + rmars_s_course3 + stars_test7 + 
  rmars_test8 + rmars_s_course1 + rmars_course5 + rmars_course1 + 
  rmars_s_course5 + rmars_s_test8

calc_adv =~ rmars_num6 + rmars_num8 + rmars_num9 + rmars_num7

maths_test =~ rmars_test10 + rmars_test9 + rmars_test2 + stars_m_test8

stats_test =~ rmars_s_test9 + rmars_s_test2 + rmars_s_test10
'

# fit_mod_efa8_pr
efa8_pr_out <- lavaan::cfa(efa8_pr_mod, data = cfa_data, estimator = "MLR", std.lv = TRUE)

efa8_pr_sum <- lavaan::summary(efa8_pr_out, fit.measure = TRUE, standardized = TRUE, rsquare = TRUE)

# mod indices
efa8_pr_mi <- lavaan::modindices(efa8_pr_out, sort = TRUE, minimum.value = 3.84) |> 
  dplyr::filter(sepc.all >= 0.25 | sepc.all <= -0.25) |>
  dplyr::arrange(desc(abs(sepc.all)))

# std resids
efa8_pr_stdresid <- lavaan::lavResiduals(efa8_pr_out, type = "raw")
efa8_pr_stdresid

#' Analysis 2: Does a four-factor model (a model where items from the STARS load onto a ‘STARS’ factor, 
#' items from the R-MARS load onto an ‘R-MARS’ factor, items from the STARS-M load onto a ‘STARS-M’ factor,
#' and items from the R-MARS-S load onto a ‘R-MARS-S’ factor) fit better than a single-factor model 
#' (where items from all four scales load onto a general mathematics/statistics anxiety factor)?

# meas_mod_four
four_factor_pr_mod <- '
stars =~ stars_test1 + stars_test2 + stars_test3 + stars_test4 + stars_test5 + stars_test6 + 
stars_test7 + stars_test8 + stars_help1 + stars_help2 + stars_help3 + stars_help4 + stars_int1 + 
stars_int2 + stars_int3 + stars_int4 + stars_int5 + stars_int6 + stars_int7 + stars_int8 + 
stars_int9 + stars_int10 + stars_int11

stars_m =~ stars_m_test1 + stars_m_test2 + stars_m_test3 + stars_m_test4 + stars_m_test5 + 
stars_m_test6 + stars_m_test7 + stars_m_test8 + stars_m_help1 + stars_m_help2 + stars_m_help3 + 
stars_m_help4 + stars_m_int1 + stars_m_int2 + stars_m_int3 + stars_m_int4 + stars_int5 + 
stars_m_int6 + stars_int7 + stars_m_int8 + stars_int9 + stars_m_int10 + stars_m_int11

rmars =~ rmars_test1 + rmars_test2 + rmars_test3 + rmars_test4 + rmars_test5 + rmars_test6 + 
rmars_test7 + rmars_test8 + rmars_test9 + rmars_test10 + rmars_num1 + rmars_num2 + rmars_num3 + 
rmars_num4 + rmars_num5 + rmars_num6 + rmars_num7 + rmars_num8 + rmars_num9 + rmars_course1 + 
rmars_course2 + rmars_course3 + rmars_course4 + rmars_course5

rmars_s =~ rmars_s_test1 + rmars_s_test2 + rmars_s_test3 + rmars_s_test4 + rmars_s_test5 + 
rmars_s_test6 + rmars_s_test7 + rmars_s_test8 + rmars_s_test9 + rmars_s_test10 + rmars_num1 + 
rmars_s_num2 + rmars_s_num3 + rmars_s_num4 + rmars_s_num5 + rmars_s_num6 + rmars_s_num8 + 
rmars_s_num9 + rmars_s_course1 + rmars_s_course2 + rmars_s_course3 + rmars_s_course4 + 
rmars_s_course5 
'

# meas_mod_one
one_factor_pr_mod <- '
superordinate =~ stars_test1 + stars_test2 + stars_test3 + stars_test4 + stars_test5 + 
stars_test6 + stars_test7 + stars_test8 + stars_help1 + stars_help2 + stars_help3 + stars_help4 + 
stars_int1 + stars_int2 + stars_int3 + stars_int4 + stars_int5 + stars_int6 + stars_int7 + 
stars_int8 + stars_int9 + stars_int10 + stars_int11 + 

stars_m_test1 + stars_m_test2 + stars_m_test3 + stars_m_test4 + stars_m_test5 + stars_m_test6 + 
stars_m_test7 + stars_m_test8 + stars_m_help1 + stars_m_help2 + stars_m_help3 + stars_m_help4 + 
stars_m_int1 + stars_m_int2 + stars_m_int3 + stars_m_int4 + stars_m_int6 + stars_m_int8 + 
stars_m_int10 + stars_m_int11 +

rmars_test1 + rmars_test2 + rmars_test3 + rmars_test4 + rmars_test5 + rmars_test6 + rmars_test7 + 
rmars_test8 + rmars_test9 + rmars_test10 + rmars_num1 + rmars_num2 + rmars_num3 + rmars_num4 + 
rmars_num5 + rmars_num6 + rmars_num7 + rmars_num8 + rmars_num9 + rmars_course1 + rmars_course2 + 

rmars_course3 + rmars_course4 + rmars_course5 + rmars_s_test1 + rmars_s_test2 + rmars_s_test3 + 
rmars_s_test4 + rmars_s_test5 + rmars_s_test6 + rmars_s_test7 + rmars_s_test8 + rmars_s_test9 + 
rmars_s_test10 + rmars_s_num2 + rmars_s_num3 + rmars_s_num4 + rmars_s_num5 + rmars_s_num6 + 
rmars_s_num8 + rmars_s_num9 + rmars_s_course1 + rmars_s_course2 + rmars_s_course3 + 
rmars_s_course4 + rmars_s_course5
'

# fit mods one & four
one_factor_pr_out <- lavaan::cfa(one_factor_pr_mod, data = cfa_data, estimator = "MLR", std.lv = TRUE)

one_factor_pr_sum <- lavaan::summary(one_factor_pr_out, fit.measure = TRUE, standardized = TRUE, rsquare = TRUE)

four_factor_pr_out <- lavaan::cfa(four_factor_pr_mod, data = cfa_data, estimator = "MLR", std.lv = TRUE)

four_factor_pr_sum <- lavaan::summary(four_factor_pr_out, fit.measure = TRUE, standardized = TRUE, rsquare = TRUE)

one_four_compare_pr <- lavaan::anova(one_factor_pr_out, four_factor_pr_out) # this is gonna' be sig anyway!

one_factor_pr_sum
four_factor_pr_sum

# mod indices
one_factor_pr_mi <- lavaan::modindices(one_factor_pr_out, sort = TRUE) 
one_factor_pr_mi

four_factor_pr_mi <- lavaan::modindices(four_factor_pr_out, sort = TRUE) 
four_factor_pr_mi

#' Analysis 4: Estimate a bifactor model and examine the variance in the manifest variables 
#' (items from the STARS, STARS-M, R-MARS, R-MARS-S scales) accounted for by a general latent 
#' factor versus lower-order latent factors (STARS, STARS-M, R-MARS, R-MARS-S subscales).

# meas_mod_bifactor
bifactor_pr_mod <- '
stars =~  stars_test1 + stars_test2 + stars_test3 + stars_test4 + 
          stars_test5 + stars_test6 + stars_test7 + stars_test8 + 
          stars_help1 + stars_help2 + stars_help3 + stars_help4 + 
          stars_int1 + stars_int2 + stars_int3 + stars_int4 + stars_int5 + 
          stars_int6 + stars_int7 + stars_int8 + stars_int9 + stars_int10 + stars_int11

stars_m =~  stars_m_test1 + stars_m_test2 + stars_m_test3 + stars_m_test4 + 
            stars_m_test5 + stars_m_test6 + stars_m_test7 + stars_m_test8 + 
            stars_m_help1 + stars_m_help2 + stars_m_help3 + stars_m_help4 + 
            stars_m_int1 + stars_m_int2 + stars_m_int3 + stars_m_int4 + 
            stars_int5 + stars_m_int6 + stars_int7 + stars_m_int8 + 
            stars_int9 + stars_m_int10 + stars_m_int11

rmars =~  rmars_test1 + rmars_test2 + rmars_test3 + rmars_test4 + rmars_test5 + 
          rmars_test6 + rmars_test7 + rmars_test8 + rmars_test9 + rmars_test10 + 
          rmars_num1 + rmars_num2 + rmars_num3 + rmars_num4 + rmars_num5 + 
          rmars_num6 + rmars_num7 + rmars_num8 + rmars_num9 + 
          rmars_course1 + rmars_course2 + rmars_course3 + rmars_course4 + rmars_course5

rmars_s =~  rmars_s_test1 + rmars_s_test2 + rmars_s_test3 + rmars_s_test4 + rmars_s_test5 + 
            rmars_s_test6 + rmars_s_test7 + rmars_s_test8 + rmars_s_test9 + rmars_s_test10 + 
            rmars_num1 + rmars_s_num2 + rmars_s_num3 + rmars_s_num4 + rmars_s_num5 + 
            rmars_s_num6 + rmars_s_num7 + rmars_s_num8 + rmars_s_num9 + 
            rmars_s_course1 + rmars_s_course2 + rmars_s_course3 + rmars_s_course4 + rmars_s_course5 

superordinate =~  stars_test1 + stars_test2 + stars_test3 + stars_test4 + 
                  stars_test5 + stars_test6 + stars_test7 + stars_test8 + 
                  stars_help1 + stars_help2 + stars_help3 + stars_help4 + 
                  stars_int1 + stars_int2 + stars_int3 + stars_int4 + stars_int5 + 
                  stars_int6 + stars_int7 + stars_int8 + stars_int9 + stars_int10 + stars_int11 + 
                  
                  stars_m_test1 + stars_m_test2 + stars_m_test3 + stars_m_test4 + 
                  stars_m_test5 + stars_m_test6 + stars_m_test7 + stars_m_test8 + 
                  stars_m_help1 + stars_m_help2 + stars_m_help3 + stars_m_help4 + 
                  stars_m_int1 + stars_m_int2 + stars_m_int3 + stars_m_int4 +
                  stars_m_int6 + stars_m_int8 + stars_m_int10 + stars_m_int11 + 
                 
                  rmars_test1 + rmars_test2 + rmars_test3 + rmars_test4 + rmars_test5 + 
                  rmars_test6 + rmars_test7 + rmars_test8 + rmars_test9 + rmars_test10 + 
                  rmars_num1 + rmars_num2 + rmars_num3 + rmars_num4 + rmars_num5 + 
                  rmars_num6 + rmars_num7 + rmars_num8 + rmars_num9 + 
                  rmars_course1 + rmars_course2 + rmars_course3 + rmars_course4 + rmars_course5 + 
                  
                  rmars_s_test1 + rmars_s_test2 + rmars_s_test3 + rmars_s_test4 + rmars_s_test5 + 
                  rmars_s_test6 + rmars_s_test7 + rmars_s_test8 + rmars_s_test9 + rmars_s_test10 + 
                  rmars_s_num2 + rmars_s_num3 + rmars_s_num4 + rmars_s_num5 + 
                  rmars_s_num6 + rmars_s_num7 + rmars_s_num8 + rmars_s_num9 + 
                  rmars_s_course1 + rmars_s_course2 + rmars_s_course3 + rmars_s_course4 + rmars_s_course5
'

# fit_mod_bifactor_pr
bifactor_pr_out <- lavaan::cfa(bifactor_pr_mod, data = cfa_data, estimator = "MLR", std.lv = TRUE, orthogonal = TRUE)

bifactor_pr_sum <- lavaan::summary(bifactor_pr_out, fit.measure = TRUE, standardized = TRUE, rsquare = TRUE)
bifactor_pr_sum

bifactor_pr_mi <- lavaan::modindices(bifactor_pr_out, sort = TRUE)

#bifactor_pr_loadings <- lavaan::parameterEstimates(bifactor_pr_out, standardized = TRUE, fmi = TRUE, output = data.frame)
bifactor_pr_table <- lavaan::parameterTable(bifactor_pr_out) |>
  dplyr::select(lhs, op, rhs, est, se) |>
  dplyr::filter(op == "=~")
bifactor_pr_table 

#' Analysis 6: Compare the single-factor, four-factor, bi-factor, and hierarchical models to establish 
#' which latent-variable structure best explains the data.

fit_subset_pr <- c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")

#null RMSEAs
efa8_nullRMSEA_pr <- semTools::nullRMSEA(efa8_pr_out)
one_factor_nullRMSEA_pr <- semTools::nullRMSEA(one_factor_pr_out)
four_factor_nullRMSEA_pr <- semTools::nullRMSEA(four_factor_pr_out)
bifactor_nullRMSEA_pr <- semTools::nullRMSEA(bifactor_pr_out)

null_RMSEA_pr <- c(efa8_nullRMSEA_pr
                , one_factor_nullRMSEA_pr
                , four_factor_nullRMSEA_pr
                , bifactor_nullRMSEA_pr
                )

cfa_fit_summary_pr <- round(rbind(lavaan::fitmeasures(efa8_pr_out, fit_subset_pr)
                           , lavaan::fitmeasures(one_factor_pr_out, fit_subset_pr)
                           , lavaan::fitmeasures(four_factor_pr_out, fit_subset_pr)
                           , lavaan::fitmeasures(bifactor_pr_out, fit_subset_pr)
                     ), 3)


fit_summary_pr <- round(cbind(cfa_fit_summary_pr
                           , null_RMSEA_pr), 3)

fit_summary_pr <- tibble::as_tibble(fit_summary_pr)

fit_summary_pr <- fit_summary_pr |> 
  dplyr::relocate(null_RMSEA_pr, .after = tli)

`CFA Model` <- c("8 Factor"
                 , "1 Factor"
                 , "4 Factor"
                 , "Bi-Factor"
                 )

fit_summary_pr <- cbind(`CFA Model`
                     , fit_summary_pr
                     )

colnames(fit_summary_pr) <- c("CFA Model"
                           , "Chi-Sq "
                           , "Degrees of Freedom"
                           , "Chi-Sq p-value" 
                           , "CFI" 
                           , "TLI"
                           , "Null RMSEA"
                           , "RMSEA" 
                           , "RMSEA 90% CI (Upper)" 
                           , "RMSEA 90% CI (Lower)"
                           , "SRMR"
                          )

fit_summary_pr <- fit_summary_pr |>
  tidyr::pivot_longer(-"CFA Model") |>
  tidyr::pivot_wider(name, names_from = "CFA Model", values_from = "value") |>
  dplyr::rename("CFA Model" = name)

fit_summary_pr