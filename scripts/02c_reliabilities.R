# read in anonymised data file
efa_data <- readr::read_csv(here::here("data/efa_data.csv"))
core_data <- readr::read_csv(here::here("data/core_data.csv"))

###### efa ######

# create dataframes containing raw scores for each original factor
stars_efa <- dplyr::select(efa_data, c(Q7.1_1:Q7.1_23))

stars_m_efa <- dplyr::select(efa_data, c(Q8.1_1:Q8.1_20, Q7.1_9, Q7.1_12, Q7.1_17))

rmars_efa <- dplyr::select(efa_data, c(Q8.2_1:Q8.2_20))

rmars_s_efa <- dplyr::select(efa_data, c(Q7.2_1:Q7.2_19, Q8.2_11))

trait_efa <- dplyr::select(efa_data, c(Q9.1_1:Q9.1_21))

test_efa <- dplyr::select(efa_data, c(Q10.1_1:Q10.1_25))

fne_efa <- dplyr::select(efa_data, c(Q11.1_1:Q11.1_8))

social_efa <- dplyr::select(efa_data, c(Q12.1_1:Q12.1_24))

creativity_efa <- dplyr::select(efa_data, c(Q13.1_1:Q13.1_8))

iou_efa <- dplyr::select(efa_data, c(Q14.1_1:Q14.1_12))

efficacy_efa <- dplyr::select(efa_data, c(Q15.1_1:Q15.1_8))

persistance_efa <- dplyr::select(efa_data, c(Q16.1_2:Q16.1_7)) # reverse scoring already handled and saved in data


# calculate the point estimate and confidence interval for omega reliability coefficient

stars_omega_efa <- MBESS::ci.reliability(stars_efa, type = "omega", conf.level = 0.95)

stars_m_omega_efa <- MBESS::ci.reliability(stars_m_efa, type = "omega", conf.level = 0.95)

rmars_omega_efa <- MBESS::ci.reliability(rmars_efa, type = "omega", conf.level = 0.95)

rmars_s_omega_efa <- MBESS::ci.reliability(rmars_s_efa, type = "omega", conf.level = 0.95)

trait_omega_efa <- MBESS::ci.reliability(trait_efa, type = "omega", conf.level = 0.95)

test_omega_efa <- MBESS::ci.reliability(test_efa, type = "omega", conf.level = 0.95)

fne_omega_efa <- MBESS::ci.reliability(fne_efa, type = "omega", conf.level = 0.95)

social_omega_efa <- MBESS::ci.reliability(social_efa, type = "omega", conf.level = 0.95)

creativity_omega_efa <- MBESS::ci.reliability(creativity_efa, type = "omega", conf.level = 0.95)

iou_omega_efa <- MBESS::ci.reliability(iou_efa, type = "omega", conf.level = 0.95)

efficacy_omega_efa <- MBESS::ci.reliability(efficacy_efa, type = "omega", conf.level = 0.95)

persistance_omega_efa <- MBESS::ci.reliability(persistance_efa, type = "omega", conf.level = 0.95) 

# get alphas for sense-check comparison

stars_alpha_efa <- psych::alpha(stars_efa)
stars_m_alpha_efa <- psych::alpha(stars_m_efa)
rmars_alpha_efa <- psych::alpha(rmars_efa)
rmars_s_alpha_efa <- psych::alpha(rmars_s_efa)
trait_alpha_efa <- psych::alpha(trait_efa)
test_alpha_efa <- psych::alpha(test_efa)
fne_alpha_efa <- psych::alpha(fne_efa)
social_alpha_efa <- psych::alpha(social_efa)
creativity_alpha_efa <- psych::alpha(creativity_efa)
iou_alpha_efa <- psych::alpha(iou_efa)
efficacy_alpha_efa <- psych::alpha(efficacy_efa)
persistance_alpha_efa <- psych::alpha(persistance_efa)

####### core #######

# create dataframes containing raw scores for each original factor
stars_core <- dplyr::select(core_data, c(Q7.1_1:Q7.1_23))

stars_m_core <- dplyr::select(core_data, c(Q8.1_1:Q8.1_20, Q7.1_9, Q7.1_12, Q7.1_17))

rmars_core <- dplyr::select(core_data, c(Q8.2_1:Q8.2_20))

rmars_s_core <- dplyr::select(core_data, c(Q7.2_1:Q7.2_19, Q8.2_11))

trait_core <- dplyr::select(core_data, c(Q9.1_1:Q9.1_21))

test_core <- dplyr::select(core_data, c(Q10.1_1:Q10.1_25))

fne_core <- dplyr::select(core_data, c(Q11.1_1:Q11.1_8))

social_core <- dplyr::select(core_data, c(Q12.1_1:Q12.1_24))

creativity_core <- dplyr::select(core_data, c(Q13.1_1:Q13.1_8))

iou_core <- dplyr::select(core_data, c(Q14.1_1:Q14.1_12))

efficacy_core <- dplyr::select(core_data, c(Q15.1_1:Q15.1_8))

persistance_core <- dplyr::select(core_data, c(Q16.1_2:Q16.1_7)) # reverse scoring already handled and saved in data


# calculate the point estimate and confidence interval for omega reliability coefficient

stars_omega_core <- MBESS::ci.reliability(stars_core, type = "omega", conf.level = 0.95)

stars_m_omega_core <- MBESS::ci.reliability(stars_m_core, type = "omega", conf.level = 0.95)

rmars_omega_core <- MBESS::ci.reliability(rmars_core, type = "omega", conf.level = 0.95)

rmars_s_omega_core <- MBESS::ci.reliability(rmars_s_core, type = "omega", conf.level = 0.95)

trait_omega_core <- MBESS::ci.reliability(trait_core, type = "omega", conf.level = 0.95)

test_omega_core <- MBESS::ci.reliability(test_core, type = "omega", conf.level = 0.95)

fne_omega_core <- MBESS::ci.reliability(fne_core, type = "omega", conf.level = 0.95)

social_omega_core <- MBESS::ci.reliability(social_core, type = "omega", conf.level = 0.95)

creativity_omega_core <- MBESS::ci.reliability(creativity_core, type = "omega", conf.level = 0.95)

iou_omega_core <- MBESS::ci.reliability(iou_core, type = "omega", conf.level = 0.95)

efficacy_omega_core <- MBESS::ci.reliability(efficacy_core, type = "omega", conf.level = 0.95)

persistance_omega_core <- MBESS::ci.reliability(persistance_core, type = "omega", conf.level = 0.95) 

# get alphas for sense-check comparison

stars_alpha_core <- psych::alpha(stars_core)
stars_m_alpha_core <- psych::alpha(stars_m_core)
rmars_alpha_core <- psych::alpha(rmars_core)
rmars_s_alpha_core <- psych::alpha(rmars_s_core)
trait_alpha_core <- psych::alpha(trait_core)
test_alpha_core <- psych::alpha(test_core)
fne_alpha_core <- psych::alpha(fne_core)
social_alpha_core <- psych::alpha(social_core)
creativity_alpha_core <- psych::alpha(creativity_core)
iou_alpha_core <- psych::alpha(iou_core)
efficacy_alpha_core <- psych::alpha(efficacy_core)
persistance_alpha_core <- psych::alpha(persistance_core)

# create tables

var_names <- c("STARS", "STARS-M", "R-MARS", "R-MARS-S", 
               "Trait Anxiety", "Test Anxiety", "Fear of Negative Evaluation", 
               "Social Anxiety", "Creativity Anxiety", 
               "Intolerance of Uncertainty", "Self Efficacy", "Persistence")

omega_efa <- c(stars_omega_efa$est, stars_m_omega_efa$est, rmars_omega_efa$est, rmars_s_omega_efa$est, trait_omega_efa$est, 
           test_omega_efa$est, fne_omega_efa$est, social_omega_efa$est, creativity_omega_efa$est, iou_omega_efa$est, 
           efficacy_omega_efa$est, persistance_omega_efa$est)

alpha_efa <- c(stars_alpha_efa$total$raw_alpha, stars_m_alpha_efa$total$raw_alpha, rmars_alpha_efa$total$raw_alpha, rmars_s_alpha_efa$total$raw_alpha, trait_alpha_efa$total$raw_alpha, 
           test_alpha_efa$total$raw_alpha, fne_alpha_efa$total$raw_alpha, social_alpha_efa$total$raw_alpha, creativity_alpha_efa$total$raw_alpha, iou_alpha_efa$total$raw_alpha, 
           efficacy_alpha_efa$total$raw_alpha, persistance_alpha_efa$total$raw_alpha)

omega_core <- c(stars_omega_core$est, stars_m_omega_core$est, rmars_omega_core$est, rmars_s_omega_core$est, trait_omega_core$est, 
                test_omega_core$est, fne_omega_core$est, social_omega_core$est, creativity_omega_core$est, iou_omega_core$est, 
                efficacy_omega_core$est, persistance_omega_core$est)

alpha_core <- c(stars_alpha_core$total$raw_alpha, stars_m_alpha_core$total$raw_alpha, rmars_alpha_core$total$raw_alpha, rmars_s_alpha_core$total$raw_alpha, trait_alpha_core$total$raw_alpha, 
                test_alpha_core$total$raw_alpha, fne_alpha_core$total$raw_alpha, social_alpha_core$total$raw_alpha, creativity_alpha_core$total$raw_alpha, iou_alpha_core$total$raw_alpha, 
                efficacy_alpha_core$total$raw_alpha, persistance_alpha_core$total$raw_alpha)

reliabilities_table <- tibble::tibble(var_names, omega_efa, omega_core)
reliabilities_table


