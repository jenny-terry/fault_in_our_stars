library(magrittr)
options(scipen = 999)
options(max.print = 100000)

# read in core data (note that the parsing issues are only to do with data types, which can be fixed as required)
et_data <- here::here("data/core_data.csv")|> readr::read_csv()

# trait anx
et_trait <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = trait_anx, 
                                    r2v1 = rmars, r2v2 = trait_anx, dep = TRUE, test = "AH", 
                                    bootstrap = FALSE, eiu = .1, eil = -.1)
et_trait 

# test anx
et_test <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = test_anx, 
                                   r2v1 = rmars, r2v2 = test_anx, dep = TRUE, test = "AH", 
                                   bootstrap = FALSE, eiu = .1, eil = -.1)
et_test 

# self_efficacy
et_efficacy <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = self_efficacy, 
                                       r2v1 = rmars, r2v2 = self_efficacy, dep = TRUE, test = "AH", 
                                       bootstrap = FALSE, eiu = .1, eil = -.1)
et_efficacy 

# persistence
et_persist <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = persistence, 
                                      r2v1 = rmars, r2v2 = persistence, dep = TRUE, test = "AH", 
                                      bootstrap = FALSE, eiu = .1, eil = -.1)
et_persist 

# crt  
et_crt <- et_crt <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = crt_n_correct, 
                                            r2v1 = rmars, r2v2 = crt_n_correct, dep = TRUE, test = "AH", 
                                            bootstrap = FALSE, eiu = .1, eil = -.1)
et_crt 

# stats grades (cont, official)
et_uni_grade <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = uni_grade_num_z, 
                                        r2v1 = rmars, r2v2 = uni_grade_num_z, dep = TRUE, test = "AH", 
                                        bootstrap = FALSE, eiu = .1, eil = -.1)
et_uni_grade 

# fne
et_fne <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = fne, 
                                  r2v1 = rmars, r2v2 = fne, dep = TRUE, test = "AH", 
                                  bootstrap = FALSE, eiu = .1, eil = -.1)
et_fne 

# iou
et_iou <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = iou, 
                                  r2v1 = rmars, r2v2 = iou, dep = TRUE, test = "AH", 
                                  bootstrap = FALSE, eiu = .1, eil = -.1)
et_iou 

# creativity_anx
et_create <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = creativity_anx, 
                                     r2v1 = rmars, r2v2 = creativity_anx, dep = TRUE, test = "AH", 
                                     bootstrap = FALSE, eiu = .1, eil = -.1)
et_create 

# social anx
et_social <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = social_anx, 
                                     r2v1 = rmars, r2v2 = social_anx, dep = TRUE, test = "AH", 
                                     bootstrap = FALSE, eiu = .1, eil = -.1)
et_social 

# stats grades (self-reported)
et_sr_grade <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = sr_grade_num_z, 
                                     r2v1 = rmars, r2v2 = sr_grade_num_z, dep = TRUE, test = "AH", 
                                     bootstrap = FALSE, eiu = .1, eil = -.1)
et_sr_grade 


# table

third_var <- c("Trait Anxiety", "Test Anxiety", "Self Efficacy", "Persistence", "Cognitive Reflection", "Statistics Grade (Official)", "Fear of Negative Evaluation", "Intolerance of Uncertainty", "Creativity Anxiety", "Social Anxiety", "Statistics Grade (Self-Reported)")

corr_diff <- c(et_trait$rsdiff, et_test$rsdiff, et_efficacy$rsdiff, et_persist$rsdiff, et_crt$rsdiff, et_uni_grade$rsdiff, et_fne$rsdiff, et_iou$rsdiff, et_create$rsdiff, et_social$rsdiff, et_sr_grade$rsdiff) |> round(3)

se_corr_diff <- c(et_trait$se, et_test$se, et_efficacy$se, et_persist$se, et_crt$se, et_uni_grade$se, et_fne$se, et_iou$se, et_create$se, et_social$se, et_sr_grade$se) |> round(3)
  
cd_90ci_low <- c(et_trait$l.ci.2a, et_test$l.ci.2a, et_efficacy$l.ci.2a, et_persist$l.ci.2a, et_crt$l.ci.2a, et_uni_grade$l.ci.2a, et_fne$l.ci.2a, et_iou$l.ci.2a, et_create$l.ci.2a, et_social$l.ci.2a, et_sr_grade$l.ci.2a) |> round(3)

cd_90ci_upp <- c(et_trait$u.ci.2a, et_test$u.ci.2a, et_efficacy$u.ci.2a, et_persist$u.ci.2a, et_crt$u.ci.2a, et_uni_grade$u.ci.2a, et_fne$u.ci.2a, et_iou$u.ci.2a, et_create$u.ci.2a, et_social$u.ci.2a, et_sr_grade$u.ci.2a) |> round(3)
  
prop_dist <- c(et_trait$pd, et_test$pd, et_efficacy$pd, et_persist$pd, et_crt$pd, et_uni_grade$pd, et_fne$pd, et_iou$pd, et_create$pd, et_social$pd, et_sr_grade$pd) |> round(3)
  
pd_95ci_low <- c(et_trait$pd.l.ci, et_test$pd.l.ci, et_efficacy$pd.l.ci, et_persist$pd.l.ci, et_crt$pd.l.ci, et_uni_grade$pd.l.ci, et_fne$pd.l.ci, et_iou$pd.l.ci, et_create$pd.l.ci, et_social$pd.l.ci, et_sr_grade$pd.l.ci) |> round(3)

pd_95ci_upp <- c(et_trait$pd.u.ci, et_test$pd.u.ci, et_efficacy$pd.u.ci, et_persist$pd.u.ci, et_crt$pd.u.ci, et_uni_grade$pd.u.ci, et_fne$pd.u.ci, et_iou$pd.u.ci, et_create$pd.u.ci, et_social$pd.u.ci, et_sr_grade$pd.u.ci) |> round(3)

p <- c(et_trait$pv, et_test$pv, et_efficacy$pv, et_persist$pv, et_crt$pv, et_uni_grade$pv, et_fne$pv, et_iou$pv, et_create$pv, et_social$pv, et_sr_grade$pv) |> round(3)

# Benjamini–Hochberg correction
# https://www.statisticshowto.com/benjamini-hochberg-procedure/
p_ben_hoch <- tibble::tibble(third_var, p) |>
  dplyr::arrange(p) |>
  dplyr::mutate(rank = c(1:11)) |>
  dplyr::mutate(prop_rank = rank/11) |>
  dplyr::mutate(p_correct = prop_rank*.05) # setting false discovery rate (q) as .05 as analog to alpha (???)
p_ben_hoch

p_correct <- c(p_ben_hoch$p_correct[1], p_ben_hoch$p_correct[2], p_ben_hoch$p_correct[7], p_ben_hoch$p_correct[11], p_ben_hoch$p_correct[3], p_ben_hoch$p_correct[4], p_ben_hoch$p_correct[8], p_ben_hoch$p_correct[5], p_ben_hoch$p_correct[9], p_ben_hoch$p_correct[10], p_ben_hoch$p_correct[6])

et_table <- tibble::tibble(third_var, corr_diff, se_corr_diff, cd_90ci_low, cd_90ci_upp, prop_dist, pd_95ci_low, pd_95ci_upp, p, p_correct)

et_table <- et_table |> dplyr::arrange(abs(p))

### EXPLORATORY ANALYSES

## STARS & STARS-M

# trait anx
et_trait_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = trait_anx, 
                                            r2v1 = stars_m, r2v2 = trait_anx, dep = TRUE, 
                                            test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_trait_stars_m

# test anx
et_test_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = test_anx, 
                                           r2v1 = stars_m, r2v2 = test_anx, dep = TRUE, 
                                           test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_test_stars_m

# self_efficacy
et_efficacy_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = self_efficacy, 
                                               r2v1 = stars_m, r2v2 = self_efficacy, dep = TRUE, 
                                               test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_efficacy_stars_m

# persistence
et_persist_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = persistence, 
                                              r2v1 = stars_m, r2v2 = persistence, dep = TRUE, 
                                              test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_persist_stars_m

# crt
et_crt_stars_m <- et_crt_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = crt_n_correct, 
                                                            r2v1 = stars_m, r2v2 = crt_n_correct, dep = TRUE, 
                                                            test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_crt_stars_m

# stats grades (cont, official)
et_uni_grade_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = uni_grade_num_z, 
                                                r2v1 = stars_m, r2v2 = uni_grade_num_z, dep = TRUE, 
                                                test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_uni_grade_stars_m

# fne
et_fne_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = fne, 
                                          r2v1 = stars_m, r2v2 = fne, dep = TRUE, 
                                          test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_fne_stars_m

# iou
et_iou_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = iou, 
                                          r2v1 = stars_m, r2v2 = iou, dep = TRUE, 
                                          test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_iou_stars_m

# creativity_anx
et_create_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = creativity_anx, 
                                             r2v1 = stars_m, r2v2 = creativity_anx, dep = TRUE, 
                                             test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_create_stars_m

# social anx
et_social_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = social_anx,
                                             r2v1 = stars_m, r2v2 = social_anx, dep = TRUE, 
                                             test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)

# stats grades (cont, self-report)
et_sr_grade_stars_m <- negligible::neg.twocors(data = et_data, r1v1 = stars, r1v2 = sr_grade_num_z, 
                                               r2v1 = stars_m, r2v2 = sr_grade_num_z, dep = TRUE, 
                                               test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
# table

third_var_stars_m <- c("Trait Anxiety", "Test Anxiety", "Self Efficacy", "Persistence", "Cognitive Reflection", "Statistics Grade (Official)", "Fear of Negative Evaluation", "Intolerance of Uncertainty", "Creativity Anxiety", "Social Anxiety", "Statistics Grades (Self-Reported)")

corr_diff_stars_m <- c(et_trait_stars_m$rsdiff, et_test_stars_m$rsdiff, et_efficacy_stars_m$rsdiff, et_persist_stars_m$rsdiff, et_crt_stars_m$rsdiff, et_uni_grade_stars_m$rsdiff, et_fne_stars_m$rsdiff, et_iou_stars_m$rsdiff, et_create_stars_m$rsdiff, et_social_stars_m$rsdiff, et_sr_grade_stars_m$rsdiff) |> round(3)

se_corr_diff_stars_m <- c(et_trait_stars_m$se, et_test_stars_m$se, et_efficacy_stars_m$se, et_persist_stars_m$se, et_crt_stars_m$se, et_uni_grade_stars_m$se, et_fne_stars_m$se, et_iou_stars_m$se, et_create_stars_m$se, et_social_stars_m$se, et_sr_grade_stars_m$se) |> round(3)
  
cd_90ci_low_stars_m <- c(et_trait_stars_m$l.ci.2a, et_test_stars_m$l.ci.2a, et_efficacy_stars_m$l.ci.2a, et_persist_stars_m$l.ci.2a, et_crt_stars_m$l.ci.2a, et_uni_grade_stars_m$l.ci.2a, et_fne_stars_m$l.ci.2a, et_iou_stars_m$l.ci.2a, et_create_stars_m$l.ci.2a, et_social_stars_m$l.ci.2a, et_sr_grade_stars_m$l.ci.2a) |> round(3)

cd_90ci_upp_stars_m <- c(et_trait_stars_m$u.ci.2a, et_test_stars_m$u.ci.2a, et_efficacy_stars_m$u.ci.2a, et_persist_stars_m$u.ci.2a, et_crt_stars_m$u.ci.2a, et_uni_grade_stars_m$u.ci.2a, et_fne_stars_m$u.ci.2a, et_iou_stars_m$u.ci.2a, et_create_stars_m$u.ci.2a, et_social_stars_m$u.ci.2a, et_sr_grade_stars_m$u.ci.2a) |> round(3)
  
prop_dist_stars_m <- c(et_trait_stars_m$pd, et_test_stars_m$pd, et_efficacy_stars_m$pd, et_persist_stars_m$pd, et_crt_stars_m$pd, et_uni_grade_stars_m$pd, et_fne_stars_m$pd, et_iou_stars_m$pd, et_create_stars_m$pd, et_social_stars_m$pd, et_sr_grade_stars_m$pd) |> round(3)
  
pd_95ci_low_stars_m <- c(et_trait_stars_m$pd.l.ci, et_test_stars_m$pd.l.ci, et_efficacy_stars_m$pd.l.ci, et_persist_stars_m$pd.l.ci, et_crt_stars_m$pd.l.ci, et_uni_grade_stars_m$pd.l.ci, et_fne_stars_m$pd.l.ci, et_iou_stars_m$pd.l.ci, et_create_stars_m$pd.l.ci, et_social_stars_m$pd.l.ci, et_sr_grade_stars_m$pd.l.ci) |> round(3)

pd_95ci_upp_stars_m <- c(et_trait_stars_m$pd.u.ci, et_test_stars_m$pd.u.ci, et_efficacy_stars_m$pd.u.ci, et_persist_stars_m$pd.u.ci, et_crt_stars_m$pd.u.ci, et_uni_grade_stars_m$pd.u.ci, et_fne_stars_m$pd.u.ci, et_iou_stars_m$pd.u.ci, et_create_stars_m$pd.u.ci, et_social_stars_m$pd.u.ci, et_sr_grade_stars_m$pd.u.ci) |> round(3)

p_stars_m <- c(et_trait_stars_m$pv, et_test_stars_m$pv, et_efficacy_stars_m$pv, et_persist_stars_m$pv, et_crt_stars_m$pv, et_uni_grade_stars_m$pv, et_fne_stars_m$pv, et_iou_stars_m$pv, et_create_stars_m$pv, et_social_stars_m$pv, et_sr_grade_stars_m$pv) |> round(3)

# Benjamini–Hochberg correct
p_ben_hoch_stars_m <- tibble::tibble(third_var_stars_m, p_stars_m) |>
  dplyr::arrange(p_stars_m) |>
  dplyr::mutate(rank = c(1:11)) |>
  dplyr::mutate(prop_rank = rank/11) |>
  dplyr::mutate(p_correct = prop_rank*.05) # setting FDR (q) as .05 as analog to alpha (???)
p_ben_hoch_stars_m

p_correct_stars_m <- c(p_ben_hoch_stars_m$p_correct[1], p_ben_hoch_stars_m$p_correct[2], p_ben_hoch_stars_m$p_correct[3], p_ben_hoch_stars_m$p_correct[4], p_ben_hoch_stars_m$p_correct[5], p_ben_hoch_stars_m$p_correct[6], p_ben_hoch_stars_m$p_correct[7], p_ben_hoch_stars_m$p_correct[8], p_ben_hoch_stars_m$p_correct[9], p_ben_hoch_stars_m$p_correct[10], p_ben_hoch_stars_m$p_correct[11])

et_table_stars_m <- tibble::tibble(third_var_stars_m, corr_diff_stars_m, se_corr_diff_stars_m, cd_90ci_low_stars_m, cd_90ci_upp_stars_m, prop_dist_stars_m, pd_95ci_low_stars_m, pd_95ci_upp_stars_m, p_stars_m, p_correct_stars_m)

et_table_stars_m <- et_table_stars_m |> dplyr::arrange(abs(p_stars_m))


## R-MARS & R-MARS-S

# trait anx
et_trait_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = trait_anx, 
                                            r2v1 = rmars_s, r2v2 = trait_anx, dep = TRUE, 
                                            test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_trait_rmars_s

# test anx
et_test_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = test_anx, 
                                           r2v1 = rmars_s, r2v2 = test_anx, dep = TRUE, 
                                           test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_test_rmars_s

# self_efficacy
et_efficacy_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = self_efficacy, 
                                               r2v1 = rmars_s, r2v2 = self_efficacy, dep = TRUE, 
                                               test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_efficacy_rmars_s

# persistence
et_persist_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = persistence, 
                                              r2v1 = rmars_s, r2v2 = persistence, dep = TRUE, 
                                              test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_persist_rmars_s

# crt
et_crt_rmars_s <- et_crt_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = crt_n_correct, 
                                                            r2v1 = rmars_s, r2v2 = crt_n_correct, dep = TRUE, 
                                                            test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_crt_rmars_s

# stats grades (cont, official)
et_uni_grade_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = uni_grade_num_z, 
                                                r2v1 = rmars_s, r2v2 = uni_grade_num_z, dep = TRUE, 
                                                test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_uni_grade_rmars_s

# fne
et_fne_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = fne, 
                                          r2v1 = rmars_s, r2v2 = fne, dep = TRUE, 
                                          test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_fne_rmars_s

# iou
et_iou_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = iou, 
                                          r2v1 = rmars_s, r2v2 = iou, dep = TRUE, 
                                          test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_iou_rmars_s

# creativity_anx
et_create_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = creativity_anx, 
                                             r2v1 = rmars_s, r2v2 = creativity_anx, dep = TRUE, 
                                             test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
et_create_rmars_s

# social anx
et_social_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = social_anx,
                                             r2v1 = rmars_s, r2v2 = social_anx, dep = TRUE, 
                                             test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)

# stats grades (cont, self-report)
et_sr_grade_rmars_s <- negligible::neg.twocors(data = et_data, r1v1 = rmars, r1v2 = sr_grade_num_z, 
                                               r2v1 = rmars_s, r2v2 = sr_grade_num_z, dep = TRUE, 
                                               test = "AH", bootstrap = FALSE, eiu = .1, eil = -.1)
# table

third_var_rmars_s <- c("Trait Anxiety", "Test Anxiety", "Self Efficacy", "Persistence", "Cognitive Reflection", "Statistics Grade (Official)", "Fear of Negative Evaluation", "Intolerance of Uncertainty", "Creativity Anxiety", "Social Anxiety", "Statistics Grades (Self-Reported)")

corr_diff_rmars_s <- c(et_trait_rmars_s$rsdiff, et_test_rmars_s$rsdiff, et_efficacy_rmars_s$rsdiff, et_persist_rmars_s$rsdiff, et_crt_rmars_s$rsdiff, et_uni_grade_rmars_s$rsdiff, et_fne_rmars_s$rsdiff, et_iou_rmars_s$rsdiff, et_create_rmars_s$rsdiff, et_social_rmars_s$rsdiff, et_sr_grade_rmars_s$rsdiff) |> round(3)

se_corr_diff_rmars_s <- c(et_trait_rmars_s$se, et_test_rmars_s$se, et_efficacy_rmars_s$se, et_persist_rmars_s$se, et_crt_rmars_s$se, et_uni_grade_rmars_s$se, et_fne_rmars_s$se, et_iou_rmars_s$se, et_create_rmars_s$se, et_social_rmars_s$se, et_sr_grade_rmars_s$se) |> round(3)

cd_90ci_low_rmars_s <- c(et_trait_rmars_s$l.ci.2a, et_test_rmars_s$l.ci.2a, et_efficacy_rmars_s$l.ci.2a, et_persist_rmars_s$l.ci.2a, et_crt_rmars_s$l.ci.2a, et_uni_grade_rmars_s$l.ci.2a, et_fne_rmars_s$l.ci.2a, et_iou_rmars_s$l.ci.2a, et_create_rmars_s$l.ci.2a, et_social_rmars_s$l.ci.2a, et_sr_grade_rmars_s$l.ci.2a) |> round(3)

cd_90ci_upp_rmars_s <- c(et_trait_rmars_s$u.ci.2a, et_test_rmars_s$u.ci.2a, et_efficacy_rmars_s$u.ci.2a, et_persist_rmars_s$u.ci.2a, et_crt_rmars_s$u.ci.2a, et_uni_grade_rmars_s$u.ci.2a, et_fne_rmars_s$u.ci.2a, et_iou_rmars_s$u.ci.2a, et_create_rmars_s$u.ci.2a, et_social_rmars_s$u.ci.2a, et_sr_grade_rmars_s$u.ci.2a) |> round(3)

prop_dist_rmars_s <- c(et_trait_rmars_s$pd, et_test_rmars_s$pd, et_efficacy_rmars_s$pd, et_persist_rmars_s$pd, et_crt_rmars_s$pd, et_uni_grade_rmars_s$pd, et_fne_rmars_s$pd, et_iou_rmars_s$pd, et_create_rmars_s$pd, et_social_rmars_s$pd, et_sr_grade_rmars_s$pd) |> round(3)

pd_95ci_low_rmars_s <- c(et_trait_rmars_s$pd.l.ci, et_test_rmars_s$pd.l.ci, et_efficacy_rmars_s$pd.l.ci, et_persist_rmars_s$pd.l.ci, et_crt_rmars_s$pd.l.ci, et_uni_grade_rmars_s$pd.l.ci, et_fne_rmars_s$pd.l.ci, et_iou_rmars_s$pd.l.ci, et_create_rmars_s$pd.l.ci, et_social_rmars_s$pd.l.ci, et_sr_grade_rmars_s$pd.l.ci) |> round(3)

pd_95ci_upp_rmars_s <- c(et_trait_rmars_s$pd.u.ci, et_test_rmars_s$pd.u.ci, et_efficacy_rmars_s$pd.u.ci, et_persist_rmars_s$pd.u.ci, et_crt_rmars_s$pd.u.ci, et_uni_grade_rmars_s$pd.u.ci, et_fne_rmars_s$pd.u.ci, et_iou_rmars_s$pd.u.ci, et_create_rmars_s$pd.u.ci, et_social_rmars_s$pd.u.ci, et_sr_grade_rmars_s$pd.u.ci) |> round(3)

p_rmars_s <- c(et_trait_rmars_s$pv, et_test_rmars_s$pv, et_efficacy_rmars_s$pv, et_persist_rmars_s$pv, et_crt_rmars_s$pv, et_uni_grade_rmars_s$pv, et_fne_rmars_s$pv, et_iou_rmars_s$pv, et_create_rmars_s$pv, et_social_rmars_s$pv, et_sr_grade_rmars_s$pv) |> round(3)

# Benjamini–Hochberg correct
p_ben_hoch_rmars_s <- tibble::tibble(third_var_rmars_s, p_rmars_s) |>
  dplyr::arrange(p_rmars_s) |>
  dplyr::mutate(rank = c(1:11)) |>
  dplyr::mutate(prop_rank = rank/11) |>
  dplyr::mutate(p_correct = prop_rank*.05) # setting FDR (q) as .05 as analog to alpha
p_ben_hoch_rmars_s

p_correct_rmars_s <- c(p_ben_hoch_rmars_s$p_correct[1], p_ben_hoch_rmars_s$p_correct[2], p_ben_hoch_rmars_s$p_correct[3], p_ben_hoch_rmars_s$p_correct[4], p_ben_hoch_rmars_s$p_correct[5], p_ben_hoch_rmars_s$p_correct[6], p_ben_hoch_rmars_s$p_correct[7], p_ben_hoch_rmars_s$p_correct[8], p_ben_hoch_rmars_s$p_correct[9], p_ben_hoch_rmars_s$p_correct[10], p_ben_hoch_rmars_s$p_correct[11])

et_table_rmars_s <- tibble::tibble(third_var_rmars_s, corr_diff_rmars_s, se_corr_diff_rmars_s, cd_90ci_low_rmars_s, cd_90ci_upp_rmars_s, prop_dist_rmars_s, pd_95ci_low_rmars_s, pd_95ci_upp_rmars_s, p_rmars_s, p_correct_rmars_s)

et_table_rmars_s <- et_table_rmars_s |> dplyr::arrange(abs(p_rmars_s))