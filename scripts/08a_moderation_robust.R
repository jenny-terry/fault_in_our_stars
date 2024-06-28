library(magrittr)
options(scipen = 999)
options(max.print = 100000)

# read in core data (note that the parsing issues are only to do with data types, which can be fixed as required)
core_data <- here::here("data/core_data.csv")|> readr::read_csv()

# select vars
mod_data <- core_data |>
  dplyr::select(university, stars, rmars)

# standardise vars
mod_data <- mod_data |>
  dplyr::mutate(stars_z = scale(stars),
                rmars_z = scale(rmars))

mod_data$stars_z <- as.vector(mod_data$stars_z)
mod_data$rmars_z <- as.vector(mod_data$rmars_z)

# set reference level
mod_data$university <- as.factor(mod_data$university)
mod_data <- within(mod_data, university <- relevel(university, ref = "University of Sussex"))

# run first model
mod_lm <- mod_data |> robust::lmRob(stars_z ~ rmars_z*university, data = _)
summary(mod_lm)

# model did not fit, so will remove n < 2 and re-run
mod_data %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data2 <- mod_data |> dplyr::filter(university != "Avila University" 
                                       & university != "Leeds Trinity University" 
                                       & university != "Menoufia University" 
                                       & university != "National and Kapodistrian University of Athens" 
                                       & university != "University of Ghana")

mod_lm2 <- mod_data2 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data2 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data3 <- mod_data2 |> dplyr::filter(university != "Sumy Makarenko State Pedagogical University")

mod_lm3 <- mod_data3 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data3 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data4 <- mod_data3 |> dplyr::filter(university != "Atma Jaya Catholic University of Indonesia" & university != "UNESP - São Paulo State University")

mod_lm4 <- mod_data4 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data4 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data5 <- mod_data4 |> dplyr::filter(university != "CHRIST (deemed to be) University" & university != "WSB University, Poznan")

mod_lm5 <- mod_data5 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data5 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data6 <- mod_data5 |> dplyr::filter(university != "University College Dublin")

mod_lm6 <- mod_data6 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data6 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data7 <- mod_data6 |> dplyr::filter(university != "University of Pécs")

mod_lm7 <- mod_data7 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data7 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data8 <- mod_data7 |> dplyr::filter(university != "Tianjin Normal University" & university != "Qufu Normal University")

mod_lm8 <- mod_data8 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data8 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data9 <- mod_data8 |> dplyr::filter(university != "University of Northern Colorado")

mod_lm9 <- mod_data9 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data9 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data10 <- mod_data9 |> dplyr::filter(university != "University of Western Australia")

mod_lm10 <- mod_data10 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data10 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data11 <- mod_data10 |> dplyr::filter(university != "University of Southern Indiana")

mod_lm11 <- mod_data11 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data11 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data12 <- mod_data11 |> dplyr::filter(university != "Manchester Metropolitan University")

mod_lm12 <- mod_data12 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data12 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data13 <- mod_data12 |> dplyr::filter(university != "University of the Philippines Visayas")

mod_lm13 <- mod_data13 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data13 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data14 <- mod_data13 |> dplyr::filter(university != "Australian National University")

mod_lm14 <- mod_data14 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data14 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data15 <- mod_data14 |> dplyr::filter(university != "Western University")

mod_lm15 <- mod_data15 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data15 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data16 <- mod_data15 |> dplyr::filter(university != "Anglia Ruskin University" & university != "LUMSA University")

mod_lm16 <- mod_data16 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data16 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data17 <- mod_data16 |> dplyr::filter(university != "University of Brighton")

mod_lm17 <- mod_data17 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data17 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data18 <- mod_data17 |> dplyr::filter(university != "Glasgow Caledonian University")

mod_lm18 <- mod_data18 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# model did not fit, so will remove next batch and re-run
mod_data18 %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(freq)

mod_data19 <- mod_data18 |> dplyr::filter(university != "University of Kassel")

mod_lm19 <- mod_data19 |> robust::lmRob(stars_z ~ rmars_z*university, data = _)

# time to try a different approach

# create variable with no. of cases per uni
mod_data_uni_n <- mod_data |> 
  dplyr::group_by(university) |> 
  dplyr::summarize(uni_n = dplyr::n())

mod_data_new <- mod_data |> 
  dplyr::left_join(mod_data_uni_n, by = c("university")) |>
  dplyr::arrange(desc(uni_n))

mod_data_new_summary <- mod_data_new |>
  dplyr::distinct(university, uni_n)
print(mod_data_new_summary, n = 7000)

# change n until it runs!
mod_data_test <- mod_data_new |> dplyr::filter(uni_n > 141) # did not run with 136, 141 is the first n that ran

mod_lm_final <- mod_data_test |> robust::lmRob(stars_z ~ rmars_z*university, data = _)
summary(mod_lm_final)

# it only runs with 10 universities!
mod_data_new %>%
  dplyr::filter(uni_n > 141) %>%
  dplyr::group_by(university) %>%
  dplyr::summarise(cnt = dplyr::n()) %>%
  dplyr::mutate(freq = formattable::percent(cnt / sum(cnt))) %>% 
  dplyr::arrange(desc(freq))