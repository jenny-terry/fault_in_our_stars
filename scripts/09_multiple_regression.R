library(magrittr)
options(scipen = 999)

# read in core data (note that the parsing issues are only to do with data types, which can be fixed as required)
core_data <- here::here("data/core_data.csv")|> readr::read_csv()

reg_data <- core_data |>
  dplyr::select(university, stars, rmars)

# trait anx
trait_lm1 <- core_data |> robust::lmRob(trait_anx ~ stars, data = _)
trait_lm2 <- core_data |> robust::lmRob(trait_anx ~ stars + rmars, data = _)

summary(trait_lm1)
summary(trait_lm2)

trait_rsqc <- abs(trait_lm1$r.squared - trait_lm2$r.squared)
trait_rsqc

# test anx
test_lm1 <- core_data |> robust::lmRob(test_anx ~ stars, data = _)
test_lm2 <- core_data |> robust::lmRob(test_anx ~ stars + rmars, data = _)

summary(test_lm1)
summary(test_lm2)

test_rsqc <- abs(test_lm1$r.squared - test_lm2$r.squared)
test_rsqc

# fne
fne_lm1 <- core_data |> robust::lmRob(fne ~ stars, data = _)
fne_lm2 <- core_data |> robust::lmRob(fne ~ stars + rmars, data = _)

summary(fne_lm1)
summary(fne_lm2)

fne_rsqc <- abs(fne_lm1$r.squared - fne_lm2$r.squared)
fne_rsqc

# social

social_lm1 <- core_data |> robust::lmRob(social_anx ~ stars, data = _)
social_lm2 <- core_data |> robust::lmRob(social_anx ~ stars + rmars, data = _)

summary(social_lm1)
summary(social_lm2)

social_rsqc <- abs(social_lm1$r.squared - social_lm2$r.squared)
social_rsqc

# creativity anx
creativity_lm1 <- core_data |> robust::lmRob(creativity_anx ~ stars, data = _)
creativity_lm2 <- core_data |> robust::lmRob(creativity_anx ~ stars + rmars, data = _)

summary(creativity_lm1)
summary(creativity_lm2)

creativity_rsqc <- abs(creativity_lm1$r.squared - creativity_lm2$r.squared)
creativity_rsqc

# iou
iou_lm1 <- core_data |> robust::lmRob(iou ~ stars, data = _)
iou_lm2 <- core_data |> robust::lmRob(iou ~ stars + rmars, data = _)

summary(iou_lm1)
summary(iou_lm2)

iou_rsqc <- abs(iou_lm1$r.squared - iou_lm2$r.squared)
iou_rsqc

# self efficacy
self_efficacy_lm1 <- core_data |> robust::lmRob(self_efficacy ~ stars, data = _)
self_efficacy_lm2 <- core_data |> robust::lmRob(self_efficacy ~ stars + rmars, data = _)

summary(self_efficacy_lm1)
summary(self_efficacy_lm2)

self_efficacy_rsqc <- abs(self_efficacy_lm1$r.squared - self_efficacy_lm2$r.squared)
self_efficacy_rsqc

# persistence
persistence_lm1 <- core_data |> robust::lmRob(persistence ~ stars, data = _)
persistence_lm2 <- core_data |> robust::lmRob(persistence ~ stars + rmars, data = _)

summary(persistence_lm1)
summary(persistence_lm2)

persistence_rsqc <- abs(persistence_lm1$r.squared - persistence_lm2$r.squared)
persistence_rsqc

# crt
crt_lm1 <- core_data |> robust::lmRob(crt_n_correct ~ stars, data = _)
crt_lm2 <- core_data |> robust::lmRob(crt_n_correct ~ stars + rmars, data = _)

summary(crt_lm1)
summary(crt_lm2)

crt_rsqc <- abs(crt_lm1$r.squared - crt_lm2$r.squared)
crt_rsqc

# stats grades (official)

uni_grade_lm1 <- core_data |> robust::lmRob(uni_grade_num_z ~ stars, data = _)
uni_grade_lm2 <- core_data |> robust::lmRob(uni_grade_num_z ~ stars + rmars, data = _)

summary(uni_grade_lm1)
summary(uni_grade_lm2)

uni_grade_rsqc <- abs(uni_grade_lm1$r.squared - uni_grade_lm2$r.squared)
uni_grade_rsqc

# stats grades (self report)

sr_grade_lm1 <- core_data |> robust::lmRob(sr_grade_num_z ~ stars, data = _)
sr_grade_lm2 <- core_data |> robust::lmRob(sr_grade_num_z ~ stars + rmars, data = _)

summary(sr_grade_lm1)
summary(sr_grade_lm2)

sr_grade_rsqc <- abs(sr_grade_lm1$r.squared - sr_grade_lm2$r.squared)
sr_grade_rsqc

# Table

rsq_m1 <- c(trait_lm1$r.squared, test_lm1$r.squared, self_efficacy_lm1$r.squared, persistence_lm1$r.squared, crt_lm1$r.squared, uni_grade_lm1$r.squared, sr_grade_lm1$r.squared, fne_lm1$r.squared, iou_lm1$r.squared, social_lm1$r.squared, creativity_lm1$r.squared) |> round(3)
rsq_m2 <- c(trait_lm2$r.squared, test_lm2$r.squared, self_efficacy_lm2$r.squared, persistence_lm2$r.squared, crt_lm2$r.squared, uni_grade_lm2$r.squared, sr_grade_lm2$r.squared, fne_lm2$r.squared, iou_lm2$r.squared, social_lm2$r.squared, creativity_lm2$r.squared) |> round(3)
rsqc <- c(trait_rsqc, test_rsqc, self_efficacy_rsqc, persistence_rsqc, crt_rsqc, uni_grade_rsqc, sr_grade_rsqc, fne_rsqc, iou_rsqc, social_rsqc, creativity_rsqc) |> round(3)

y <- c("Trait Anxiety", "Test Anxiety", "Self Efficacy", "Persistence", "Cognitive Reflection", "Statistics Grade (Official)", "Statistics Grade (Self Report)", "Fear of Negative Evaluation", "Intolerance of Uncertainty", "Social Anxiety", "Creativity Anxiety")

rsqc_table_rob <- tibble::tibble(y, rsq_m1, rsq_m2, rsqc) |>
  dplyr::mutate(rsqc_pc = round(rsqc/rsq_m1*100, 2)) |> 
  dplyr::arrange(rsqc_pc)
rsqc_table_rob

##### NON-ROBUST MODELS #####

# trait anx
trait_lm1_nonrob <- core_data |> lm(trait_anx ~ stars, data = _)
trait_lm2_nonrob <- core_data |> lm(trait_anx ~ stars + rmars, data = _)

summary(trait_lm1_nonrob)
summary(trait_lm2_nonrob)

trait_rsqc_nonrob <- abs(summary(trait_lm1_nonrob)$r.squared - summary(trait_lm2_nonrob)$r.squared)
trait_rsqc_nonrob

# test anx
test_lm1_nonrob <- core_data |> lm(test_anx ~ stars, data = _)
test_lm2_nonrob <- core_data |> lm(test_anx ~ stars + rmars, data = _)

summary(test_lm1_nonrob)
summary(test_lm2_nonrob)

test_rsqc_nonrob <- abs(summary(test_lm1_nonrob)$r.squared - summary(test_lm2_nonrob)$r.squared)
test_rsqc_nonrob

# fne
fne_lm1_nonrob <- core_data |> lm(fne ~ stars, data = _)
fne_lm2_nonrob <- core_data |> lm(fne ~ stars + rmars, data = _)

summary(fne_lm1_nonrob)
summary(fne_lm2_nonrob)

fne_rsqc_nonrob <- abs(summary(fne_lm1_nonrob)$r.squared - summary(fne_lm2_nonrob)$r.squared)
fne_rsqc_nonrob

# social

social_lm1_nonrob <- core_data |> lm(social_anx ~ stars, data = _)
social_lm2_nonrob <- core_data |> lm(social_anx ~ stars + rmars, data = _)

summary(social_lm1_nonrob)
summary(social_lm2_nonrob)

social_rsqc_nonrob <- abs(summary(social_lm1_nonrob)$r.squared - summary(social_lm2_nonrob)$r.squared)
social_rsqc_nonrob

# creativity anx
creativity_lm1_nonrob <- core_data |> lm(creativity_anx ~ stars, data = _)
creativity_lm2_nonrob <- core_data |> lm(creativity_anx ~ stars + rmars, data = _)

summary(creativity_lm1_nonrob)
summary(creativity_lm2_nonrob)

creativity_rsqc_nonrob <- abs(summary(creativity_lm1_nonrob)$r.squared - summary(creativity_lm2_nonrob)$r.squared)
creativity_rsqc_nonrob

# iou
iou_lm1_nonrob <- core_data |> lm(iou ~ stars, data = _)
iou_lm2_nonrob <- core_data |> lm(iou ~ stars + rmars, data = _)

summary(iou_lm1_nonrob)
summary(iou_lm2_nonrob)

iou_rsqc_nonrob <- abs(summary(iou_lm1_nonrob)$r.squared - summary(iou_lm2_nonrob)$r.squared)
iou_rsqc_nonrob

# self efficacy
self_efficacy_lm1_nonrob <- core_data |> lm(self_efficacy ~ stars, data = _)
self_efficacy_lm2_nonrob <- core_data |> lm(self_efficacy ~ stars + rmars, data = _)

summary(self_efficacy_lm1_nonrob)
summary(self_efficacy_lm2_nonrob)

self_efficacy_rsqc_nonrob <- abs(summary(self_efficacy_lm1_nonrob)$r.squared - summary(self_efficacy_lm2_nonrob)$r.squared)
self_efficacy_rsqc_nonrob

# persistence
persistence_lm1_nonrob <- core_data |> lm(persistence ~ stars, data = _)
persistence_lm2_nonrob <- core_data |> lm(persistence ~ stars + rmars, data = _)

summary(persistence_lm1_nonrob)
summary(persistence_lm2_nonrob)

persistence_rsqc_nonrob <- abs(summary(persistence_lm1_nonrob)$r.squared - summary(persistence_lm2_nonrob)$r.squared)
persistence_rsqc_nonrob

# crt
crt_lm1_nonrob <- core_data |> lm(crt_n_correct ~ stars, data = _)
crt_lm2_nonrob <- core_data |> lm(crt_n_correct ~ stars + rmars, data = _)

summary(crt_lm1_nonrob)
summary(crt_lm2_nonrob)

crt_rsqc_nonrob <- abs(summary(crt_lm1_nonrob)$r.squared - summary(crt_lm2_nonrob)$r.squared)
crt_rsqc_nonrob

# stats grades (official)

uni_grade_lm1_nonrob <- core_data |> lm(uni_grade_num_z ~ stars, data = _)
uni_grade_lm2_nonrob <- core_data |> lm(uni_grade_num_z ~ stars + rmars, data = _)

summary(uni_grade_lm1_nonrob)
summary(uni_grade_lm2_nonrob)

uni_grade_rsqc_nonrob <- abs(summary(uni_grade_lm1_nonrob)$r.squared - summary(uni_grade_lm2_nonrob)$r.squared)
uni_grade_rsqc_nonrob

# stats grades (self report)

sr_grade_lm1_nonrob <- core_data |> lm(sr_grade_num_z ~ stars, data = _)
sr_grade_lm2_nonrob <- core_data |> lm(sr_grade_num_z ~ stars + rmars, data = _)

summary(sr_grade_lm1_nonrob)
summary(sr_grade_lm2_nonrob)

sr_grade_rsqc_nonrob <- abs(summary(sr_grade_lm1_nonrob)$r.squared - summary(sr_grade_lm2_nonrob)$r.squared)
sr_grade_rsqc_nonrob

# Table

rsq_m1_nonrob <- c(summary(trait_lm1_nonrob)$r.squared, summary(test_lm1_nonrob)$r.squared, summary(self_efficacy_lm1_nonrob)$r.squared, summary(persistence_lm1_nonrob)$r.squared, summary(crt_lm1_nonrob)$r.squared, summary(uni_grade_lm1_nonrob)$r.squared, summary(sr_grade_lm1_nonrob)$r.squared, summary(fne_lm1_nonrob)$r.squared, summary(iou_lm1_nonrob)$r.squared, summary(social_lm1_nonrob)$r.squared, summary(creativity_lm1_nonrob)$r.squared) |> round(3)
rsq_m2_nonrob <- c(summary(trait_lm2_nonrob)$r.squared, summary(test_lm2_nonrob)$r.squared, summary(self_efficacy_lm2_nonrob)$r.squared, summary(persistence_lm2_nonrob)$r.squared, summary(crt_lm2_nonrob)$r.squared, summary(uni_grade_lm2_nonrob)$r.squared, summary(sr_grade_lm2_nonrob)$r.squared, summary(fne_lm2_nonrob)$r.squared, summary(iou_lm2_nonrob)$r.squared, summary(social_lm2_nonrob)$r.squared, summary(creativity_lm2_nonrob)$r.squared) |> round(3)
rsqc_nonrob <- c(trait_rsqc_nonrob, test_rsqc_nonrob, self_efficacy_rsqc_nonrob, persistence_rsqc_nonrob, crt_rsqc_nonrob, uni_grade_rsqc_nonrob, sr_grade_rsqc_nonrob, fne_rsqc_nonrob, iou_rsqc_nonrob, social_rsqc_nonrob, creativity_rsqc_nonrob) |> round(3)

y_nonrob <- c("Trait Anxiety", "Test Anxiety", "Self Efficacy", "Persistence", "Cognitive Reflection", "Statistics Grade (Official)", "Statistics Grade (Self Report)", "Fear of Negative Evaluation", "Intolerance of Uncertainty", "Social Anxiety", "Creativity Anxiety")

rsqc_table_nonrob <- tibble::tibble(y_nonrob, rsq_m1_nonrob, rsq_m2_nonrob, rsqc_nonrob) |>
  dplyr::mutate(rsqc_pc_nonrob = round(rsqc_nonrob/rsq_m1_nonrob*100, 3)) |> 
  dplyr::arrange(rsqc_pc_nonrob)
rsqc_table_nonrob