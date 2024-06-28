##### EFA SAMPLE MISSING DATA #####

library(magrittr)

# read in efa data
efa_data <- here::here("data/efa_data.csv")|> readr::read_csv()

### ITEM LEVEL MISSING DATA

# check missing data at item level
efa_item_missing <- efa_data %>% 
  dplyr::select(c(Q7.1_1:Q17.3)) %>%
  finalfit::ff_glimpse()

efa_item_missing_max <- max(efa_item_missing$Continuous$missing_percent)

# check % item missing per scale
efa_stars_item_missing <- efa_data %>%
  dplyr::select(c(Q7.1_1:Q7.1_24)) %>%
  naniar::pct_miss()

efa_rmars_s_item_missing <- efa_data %>%
  dplyr::select(c(Q7.2_1:Q7.2_19, Q8.2_11)) %>%
  naniar::pct_miss()

efa_stars_m_item_missing <- efa_data %>%
  dplyr::select(c(Q8.1_1:Q8.1_20, Q7.1_9, Q7.1_12, Q7.1_17)) %>%
  naniar::pct_miss()

efa_rmars_item_missing <- efa_data %>%
  dplyr::select(c(Q8.2_1:Q8.2_20)) %>%
  naniar::pct_miss()

efa_trait_item_missing <- efa_data %>%
  dplyr::select(c(Q9.1_1:Q9.1_21)) %>%
  naniar::pct_miss()

efa_test_item_missing <- efa_data %>%
  dplyr::select(c(Q10.1_1:Q10.1_25)) %>%
  naniar::pct_miss()

efa_fne_item_missing <- efa_data %>%
  dplyr::select(c(Q11.1_1:Q11.1_8)) %>%
  naniar::pct_miss()

efa_social_item_missing <- efa_data %>%
  dplyr::select(c(Q12.1_1:Q12.1_24)) %>%
  naniar::pct_miss()

efa_creativity_item_missing <- efa_data %>%
  dplyr::select(c(Q13.1_1:Q13.1_8)) %>%
  naniar::pct_miss()

efa_iou_item_missing <- efa_data %>%
  dplyr::select(c(Q14.1_1:Q14.1_12)) %>%
  naniar::pct_miss()

efa_efficacy_item_missing <- efa_data %>%
  dplyr::select(c(Q15.1_1:Q15.1_8)) %>%
  naniar::pct_miss()

efa_persist_item_missing <- efa_data %>%
  dplyr::select(c(Q16.1_2:Q16.1_7)) %>%
  naniar::pct_miss()

efa_crt_item_missing <- efa_data %>%
  dplyr::select(c(Q17.1:Q17.3)) %>%
  naniar::pct_miss()

efa_uni_grade_item_missing <- efa_data %>%
  dplyr::select(c(uni_grade_num_z)) %>%
  naniar::pct_miss()

efa_sr_grade_item_missing <- efa_data %>%
  dplyr::select(c(sr_grade_num_z)) %>%
  naniar::pct_miss()

### COMPOSITE SCORING

# (don't need composites for the EFA, but calculating anyway as it can't hurt to
# check levels of various things are similar)

# stars
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars = mean(dplyr::c_across(Q7.1_1:Q7.1_23), na.rm = TRUE))

# stars - int
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_int = mean(dplyr::c_across(c(Q7.1_2, Q7.1_5, Q7.1_6, Q7.1_7, Q7.1_9, Q7.1_11, Q7.1_12, Q7.1_14, Q7.1_17, Q7.1_18, Q7.1_20)), na.rm = TRUE))

# stars - test
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_test = mean(dplyr::c_across(c(Q7.1_1, Q7.1_4, Q7.1_8, Q7.1_10, Q7.1_13, Q7.1_15, Q7.1_21, Q7.1_22)), na.rm = TRUE))

# stars - help
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_help = mean(dplyr::c_across(c(Q7.1_3, Q7.1_16, Q7.1_19, Q7.1_23)), na.rm = TRUE))

# rmars_s
efa_data <- efa_data %>%
  dplyr::mutate(rmars_s = mean(dplyr::c_across(c(Q7.2_1:Q7.2_19, Q8.2_11)), na.rm = TRUE))

# rmars_s - test
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_s_test = mean(dplyr::c_across(c(Q7.2_1:Q7.2_10)), na.rm = TRUE))

# rmars_s - num v2
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_s_num2 = mean(dplyr::c_across(c(Q7.1_11, Q7.2_11:Q7.2_14)), na.rm = TRUE))

# rmars_s - course
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_s_course = mean(dplyr::c_across(c(Q7.2_15:Q7.2_19)), na.rm = TRUE))

# rmars_s - num v1
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_s_num1 = mean(dplyr::c_across(c(Q7.2_20:Q7.2_23)), na.rm = TRUE))

# stars_m
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_m = mean(dplyr::c_across(c(Q8.1_1:Q8.1_20, Q7.1_9, Q7.1_12, Q7.1_17)), na.rm = TRUE))

# stars_m - int
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_m_int = mean(dplyr::c_across(c(Q8.1_2, Q8.1_5, Q8.1_6, Q8.1_7, Q7.1_9, Q8.1_10, Q7.1_12, Q8.1_12, Q7.1_17, Q8.1_15, Q8.1_17)), na.rm = TRUE))

# stars_m - test
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_m_test = mean(dplyr::c_across(c(Q8.1_1, Q8.1_4, Q8.1_8, Q8.1_9, Q8.1_11, Q8.1_13, Q8.1_18, Q8.1_19)), na.rm = TRUE))

# stars_m - help
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(stars_m_help = mean(dplyr::c_across(c(Q8.1_3, Q8.1_14, Q8.1_16, Q8.1_20)), na.rm = TRUE))

# rmars
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars = mean(dplyr::c_across(Q8.2_1:Q8.2_20), na.rm = TRUE))

# rmars - test
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_test = mean(dplyr::c_across(c(Q8.2_1:Q8.2_10)), na.rm = TRUE))

# rmars - num v1
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_num2 = mean(dplyr::c_across(c(Q8.2_11:Q8.2_15)), na.rm = TRUE))

# rmars - course
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_course = mean(dplyr::c_across(c(Q8.2_16:Q8.2_20)), na.rm = TRUE))

# rmars - num v1
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(rmars_num1 = mean(dplyr::c_across(c(Q8.2_21:Q8.2_24)), na.rm = TRUE))

# trait_anx
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(trait_anx = mean(dplyr::c_across(Q9.1_1:Q9.1_21), na.rm = TRUE))

# trait_som
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(trait_som = mean(dplyr::c_across(c(Q9.1_1:Q9.1_2, Q9.1_6:Q9.1_9, Q9.1_12, Q9.1_14:Q9.1_15, Q9.1_18, Q9.1_20:Q9.1_21)), na.rm = TRUE))

# trait_cog
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(trait_cog = mean(dplyr::c_across(c(Q9.1_3:Q9.1_5, Q9.1_9:Q9.1_11, Q9.1_13, Q9.1_6:Q9.1_17, Q9.1_19)), na.rm = TRUE))

# test_anx
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(test_anx = mean(dplyr::c_across(Q10.1_1:Q10.1_25), na.rm = TRUE))

# test_anx - worry
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(test_worry = mean(dplyr::c_across(c(Q10.1_1:Q10.1_3, Q10.1_8, Q10.1_11, Q10.1_14, Q10.1_24)), na.rm = TRUE))

# test_anx - test-irrelevant
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(test_irrel = mean(dplyr::c_across(c(Q10.1_7, Q10.1_9, Q10.1_15, Q10.1_16, Q10.1_23)), na.rm = TRUE))

# test_anx - tension
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(test_tens = mean(dplyr::c_across(c(Q10.1_4:Q10.1_6, Q10.1_12, Q10.1_13, Q10.1_25)), na.rm = TRUE))

# test_anx - bodily symptoms
efa_data  <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(test_bod = mean(dplyr::c_across(c(Q10.1_10, Q10.1_17:Q10.1_22)), na.rm = TRUE))

# fne
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(fne = mean(dplyr::c_across(Q11.1_1:Q11.1_8), na.rm = TRUE))

# social_anx
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(social_anx = mean(dplyr::c_across(Q12.1_1:Q12.1_24), na.rm = TRUE))

# social_anx - performance
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(social_perf = mean(dplyr::c_across(c(Q12.1_1:Q12.1_4, Q12.1_6, Q12.1_8, Q12.1_9, Q12.1_13, Q12.1_14, Q12.1_16, Q12.1_17, Q12.1_20, Q12.1_21)), na.rm = TRUE))

# social_anx - social interaction
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(social_inter = mean(dplyr::c_across(c(Q12.1_5, Q12.1_7, Q12.1_10:Q12.1_12, Q12.1_15, Q12.1_18, Q12.1_19, Q12.1_22:Q12.1_24)), na.rm = TRUE))

# creativity_anx
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(creativity_anx = mean(dplyr::c_across(Q13.1_1:Q13.1_8), na.rm = TRUE))

# non_creativity_anx
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(non_creativity_anx = mean(dplyr::c_across(Q13.1_10:Q13.1_16), na.rm = TRUE))

# iou
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(iou = mean(dplyr::c_across(Q14.1_1:Q14.1_12), na.rm = TRUE))

# iou - prospective
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(iou_pros = mean(dplyr::c_across(c(Q14.1_1:Q14.1_2, Q14.1_4:Q14.1_5, Q14.1_8:Q14.1_9, Q14.1_11)), na.rm = TRUE))

# iou - inhibitory
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(iou_inhib = mean(dplyr::c_across(c(Q14.1_3, Q14.1_6:Q14.1_7, Q14.1_10, Q14.1_12)), na.rm = TRUE))

# self_efficacy
efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(self_efficacy = mean(dplyr::c_across(Q15.1_1:Q15.1_8), na.rm = TRUE))

# persistence
efa_data <- efa_data %>% 
  dplyr::mutate(Q16.1_2 = dplyr::recode(Q16.1_2, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                Q16.1_3 = dplyr::recode(Q16.1_3, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                Q16.1_4 = dplyr::recode(Q16.1_4, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                Q16.1_5 = dplyr::recode(Q16.1_5, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1),
                Q16.1_7 = dplyr::recode(Q16.1_7, `1` = 5, `2` = 4, `3` = 3, `4` = 2, `5` = 1))

efa_data <- efa_data |> 
  dplyr::rowwise() |> 
  dplyr::mutate(persistence = mean(dplyr::c_across(Q16.1_1:Q16.1_7), na.rm = TRUE))

# crt (code credit: Omid Ghasemi)
efa_data <- efa_data |> 
  dplyr::mutate(crt_n_correct = rowSums(cbind(Q17.1 == 4, Q17.2 == 10, Q17.3 == 39), na.rm = TRUE),
                crt_n_intuitive = rowSums(cbind(Q17.1 == 8, Q17.2 == 50, Q17.3 == 20), na.rm = TRUE),
                crt_n_other = rowSums(is.na(cbind(Q17.1, Q17.2, Q17.3))|
                                        cbind(!(Q17.1 == 4 | Q17.1 == 8),
                                              !(Q17.2 == 10 | Q17.2 == 50),
                                              !(Q17.3 == 20 | Q17.3 == 39)), na.rm = TRUE)) 

#readr::write_csv(efa_data, here::here("data/efa_data.csv"))

### COMPOSITE LEVEL MISSING

efa_stars_comp_missing <- efa_data %>%
  dplyr::select(stars) %>%
  naniar::pct_miss()

efa_rmars_s_comp_missing <- efa_data %>%
  dplyr::select(rmars_s) %>%
  naniar::pct_miss()

efa_stars_m_comp_missing <- efa_data %>%
  dplyr::select(stars_m) %>%
  naniar::pct_miss()

efa_rmars_comp_missing <- efa_data %>%
  dplyr::select(rmars) %>%
  naniar::pct_miss()

efa_trait_comp_missing <- efa_data %>%
  dplyr::select(trait_anx) %>%
  naniar::pct_miss()

efa_test_comp_missing <- efa_data %>%
  dplyr::select(test_anx) %>%
  naniar::pct_miss()

efa_fne_comp_missing <- efa_data %>%
  dplyr::select(fne) %>%
  naniar::pct_miss()

efa_social_comp_missing <- efa_data %>%
  dplyr::select(social_anx) %>%
  naniar::pct_miss()

efa_creativity_comp_missing <- efa_data %>%
  dplyr::select(creativity_anx) %>%
  naniar::pct_miss()

efa_iou_comp_missing <- efa_data %>%
  dplyr::select(iou) %>%
  naniar::pct_miss()

efa_efficacy_comp_missing <- efa_data %>%
  dplyr::select(self_efficacy) %>%
  naniar::pct_miss()

efa_persist_comp_missing <- efa_data %>%
  dplyr::select(persistence) %>%
  naniar::pct_miss()

efa_crt_comp_missing <- efa_data %>%
  dplyr::select(crt_n_correct) %>%
  naniar::pct_miss()

efa_uni_grade_comp_missing <- efa_data %>%
  dplyr::select(uni_grade_num_z) %>%
  naniar::pct_miss()

efa_sr_grade_comp_missing <- efa_data %>%
  dplyr::select(sr_grade_num_z) %>%
  naniar::pct_miss()

efa_var_names <- c("STARS", "R-MARS-S", "STARS-M", "R-MARS", 
                    "Trait Anxiety", "Test Anxiety", "Fear of Negative Evaluation", "Social Anxiety", 
                    "Creativity Anxiety", "Intolerance of Uncertainty", "Self Efficacy", "Persistence", 
                    "Cognitive Reflection", "Statistics Grade (Official)", "Statistics Grade (Self-Reported)")
efa_percent_item_missing <- c(efa_stars_item_missing, efa_rmars_s_item_missing, efa_stars_m_item_missing, efa_rmars_item_missing,
                               efa_trait_item_missing, efa_test_item_missing, efa_fne_item_missing, efa_social_item_missing,
                               efa_creativity_item_missing, efa_iou_item_missing, efa_efficacy_item_missing, efa_persist_item_missing,
                               efa_crt_item_missing, efa_uni_grade_item_missing, efa_sr_grade_item_missing) |> round (3)
efa_percent_comp_missing <- c(efa_stars_comp_missing, efa_rmars_s_comp_missing, efa_stars_m_comp_missing, efa_rmars_comp_missing,
                               efa_trait_comp_missing, efa_test_comp_missing, efa_fne_comp_missing, efa_social_comp_missing,
                               efa_creativity_comp_missing, efa_iou_comp_missing, efa_efficacy_comp_missing, efa_persist_comp_missing,
                               efa_crt_comp_missing, efa_uni_grade_comp_missing, efa_sr_grade_comp_missing) |> round (3)
efa_missing_table <- tibble::tibble(efa_var_names, efa_percent_item_missing, efa_percent_comp_missing) |> 
  dplyr::arrange(abs(efa_percent_item_missing))


