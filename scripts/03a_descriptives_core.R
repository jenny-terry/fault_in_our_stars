##### CORE SAMPLE DESCRIPTIVE STATISTICS #####

library(magrittr)

# read in core data
core_data <- here::here("data/core_data.csv")|> readr::read_csv()

### COMPOSITE LEVEL

# select and rename for table
core_comp_vars <- core_data |>
  dplyr::select(stars, stars_m, rmars, rmars_s, trait_anx, test_anx, fne, 
                social_anx, creativity_anx, iou, self_efficacy, persistence, crt_n_correct, 
                uni_grade_num, stats_edu_mean_grade, uni_grade_num_z, sr_grade_num_z) |>
  dplyr::rename(STARS = stars
                , "R-MARS" = rmars
                , "STARS-M" = stars_m
                , "R-MARS-S" = rmars_s
                , "Trait Anxiety" = trait_anx
                , "Test Anxiety" = test_anx
                , "Fear of Negative Evaluation" = fne
                , "Social Anxiety" = social_anx
                , "Creativity Anxiety" = creativity_anx
                , "Intolerance of Uncertainty" = iou
                , "Self Efficacy" = self_efficacy
                , Persistence = persistence
                , "Cognitive Reflection" = crt_n_correct
                , "Statistics Grade (Official)" = uni_grade_num
                , "Statistics Grade (Self-Report)" = stats_edu_mean_grade
                , "Statistics Grade (Official) - Standardised" = uni_grade_num_z
                , "Statistics Grade (Self-Report) - Standardised" = sr_grade_num_z
                )

core_comp_summary <- psych::describe(core_comp_vars)
core_comp_summary$item <- row.names(core_comp_summary)  
core_comp_summary <- tibble::as_tibble(core_comp_summary) 
core_comp_summary <- core_comp_summary |>
  dplyr::select(item, mean, sd, min, max, skew, kurtosis, se)
core_comp_summary

### SUBSCALE LEVEL

# select and rename for table
core_subs_vars <- core_data |>
  dplyr::select(stars_int, stars_help, stars_test
                , stars_m_int, stars_m_help, stars_m_test
                , rmars_test, rmars_course, rmars_num1, rmars_num2
                , rmars_s_test, rmars_s_course, rmars_s_num1, rmars_s_num2 
                , trait_som, trait_cog
                , test_worry, test_irrel, test_tens, test_bod
                , social_perf, social_inter
                , iou_pros, iou_inhib
                ) |>
  dplyr::rename("STARS - Interpretation" = stars_int
                , "STARS - Asking for Help" = stars_help
                , "STARS - Test" = stars_test
                
                , "STARS-M - Interpretation" = stars_m_int
                , "STARS-M - Asking for Help" = stars_m_help
                , "STARS-M - Test" = stars_m_test
                
                , "R-MARS - Test" = rmars_test
                , "R-MARS - Course" = rmars_course
                , "R-MARS - Numeric Tasks" = rmars_num1
                , "R-MARS - Numeric Tasks (Modified)" = rmars_num2
                
                , "R-MARS-S - Test" = rmars_s_test
                , "R-MARS-S - Course" = rmars_s_course
                , "R-MARS-S - Numeric Tasks" = rmars_s_num1
                , "R-MARS-S - Numeric Tasks (Modified)" = rmars_s_num2
                
                , "Trait - Somatic" = trait_som
                , "Trait - Cognitive" = trait_cog
                
                , "Test - Worry" = test_worry
                , "Test - Irrelevant Thoughts" = test_irrel
                , "Test - Tension" = test_tens
                , "Test - Bodily Symptoms" = test_bod
                
                , "Social - Performance" = social_perf
                , "Social - Interaction" = social_inter
                
                , "Intolerance of Uncertainty - Prospective" = iou_pros
                , "Intolerance of Uncertainty - Inhibitory" = iou_inhib
                )

core_subs_summary <- psych::describe(core_subs_vars)
core_subs_summary$item <- row.names(core_subs_summary)  
core_subs_summary <- tibble::as_tibble(core_subs_summary) 
core_subs_summary <- core_subs_summary |>
  dplyr::select(item, mean, sd, min, max, skew, kurtosis, se)
core_subs_summary

### COMPOSITE LEVEL CORRELATIONS

core_comp_vars_corr <- core_data |>
  dplyr::select(stars, stars_m, rmars, rmars_s, trait_anx, test_anx, fne, 
                social_anx, creativity_anx, iou, self_efficacy, persistence, crt_n_correct, 
                uni_grade_num_z, sr_grade_num_z)

core_comp_vars_corr_names <- c("stars", "stars_m", "rmars", "rmars_s", "trait_anx", "test_anx", "fne", 
"social_anx", "creativity_anx", "iou", "self_efficacy", "persistence", "crt_n_correct")

core_comp_corr <- lavaan::lavCor(object = core_comp_vars_corr, 
                           ordered = names(core_comp_vars_corr_names),
                           output = "cor")
core_comp_corr

### SUBSCALE LEVEL CORRELATIONS
# note to self - add in comps as well to get corrs with those

core_subs_vars_corr <- core_data |>
  dplyr::select(stars_int, stars_help, stars_test
                , stars_m_int, stars_m_help, stars_m_test
                , rmars_test, rmars_course, rmars_num1, rmars_num2
                , rmars_s_test, rmars_s_course, rmars_s_num1, rmars_s_num2 
                , trait_som, trait_cog
                , test_worry, test_irrel, test_tens, test_bod
                , social_perf, social_inter
                , iou_pros, iou_inhib
                )

core_subs_vars_corr_names <- c("stars_int", "stars_help", "stars_test"
                               , "stars_m_int", "stars_m_help", "stars_m_test"
                               , "rmars_test", "rmars_course", "rmars_num1", "rmars_num2"
                               , "rmars_s_test", "rmars_s_course", "rmars_s_num1", "rmars_s_num2"
                               , "trait_som", "trait_cog"
                               , "test_worry", "test_irrel", "test_tens", "test_bod"
                               , "social_perf", "social_inter"
                               , "iou_pros", "iou_inhib"
                               )

core_subs_corr <- lavaan::lavCor(object = core_subs_vars_corr, 
                                 ordered = names(core_subs_vars_corr_names),
                                 output = "cor")
core_subs_corr
