library(magrittr)

# read in data (note that the parsing issues are only to do with data types, which can be fixed as required)
smarvus_data <- here::here("data/smarvus_complete_050524_st.csv")|> 
  readr::read_csv()

##### EXCLUSIONS #####

# (duplicates and empty cases have already been removed)

# pre-reg'd exclusions first

# count & exclude any post-graduates (keeping NAs due to length of survey)
n_postgrads <- smarvus_data |>
  dplyr::filter(degree_year == "Postgraduate") |>
  nrow()

n_other <- smarvus_data |>
  dplyr::filter(degree_year == "Other") |>
  nrow()

smarvus_data <- smarvus_data |> 
  dplyr::filter(degree_year == "1st Year" | degree_year == "2nd Year" | degree_year == "3rd Year" | 
                degree_year == "4th Year" | degree_year == "5th Year" | is.na(degree_year))

smarvus_data |> dplyr::distinct(degree_year)

# count & exclude students on maths/sciences degrees

n_maths <- smarvus_data |> 
  dplyr::filter(degree_major == "Maths") |> 
  nrow()

smarvus_data <- smarvus_data |> 
  dplyr::filter(degree_major != "Maths" | is.na(degree_major))

n_science <- smarvus_data |> 
  dplyr::filter(degree_major == "Sciences") |> 
  nrow()

smarvus_data <- smarvus_data |> 
  dplyr::filter(degree_major != "Sciences" | is.na(degree_major))

smarvus_data |> dplyr::distinct(degree_major)

# exclude students taking additional maths modules (must match 'yes' and have NOT specified 
# a maths-related module, except statistics)
# also excluding NAs here (only n = 26) and other unclear responses

# create new variable 
smarvus_data <- smarvus_data |> dplyr::mutate(extra_maths = dplyr::case_when(
  degree_level_maths == "No" ~ "No",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "Statistics" ~ "No",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "Unrelated" ~ "No",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "None" ~ "No",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "Maths" ~ "Yes",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "Maths Education" ~ "Yes",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "Maths Adjacent" ~ "Yes",
  degree_level_maths == "Yes (please specify)" & degree_level_maths_specify == "Unclear" ~ "Yes",
  degree_level_maths == "Yes (please specify)" & is.na(degree_level_maths_specify) ~ "Yes"))

# count and remove cases where new variable == "Yes"
n_extra_maths <- smarvus_data |> 
  dplyr::filter(extra_maths == "Yes") |> 
  nrow()

smarvus_data <- smarvus_data |> 
  dplyr::filter(extra_maths == "No")

smarvus_data |> dplyr::distinct(degree_level_maths, degree_level_maths_specify, extra_maths)

# count & exclude students not clearly studying a statistics module
# ***NOTE TYPO IN HUMANITIES***

n_no_stats <- smarvus_data |> 
  dplyr::filter(degree_major == "Arts & Humantities" | degree_major == "Uncategorised") |> 
  nrow()

smarvus_data <- smarvus_data |> 
  dplyr::filter(degree_major != "Arts & Humantities" | is.na(degree_major))

smarvus_data <- smarvus_data |> 
  dplyr::filter(degree_major != "Uncategorised" | is.na(degree_major))

n_na_degree_major <- smarvus_data |> dplyr::filter(is.na(degree_major)) |> nrow()

smarvus_data %>%
  dplyr::filter(is.na(degree_major)) |>
  dplyr::group_by(university) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(percent))

# most missing data on the degree major variable is Menoufia, which is problematic because they didn't
# have screening criteria in place and there were responses from students that clearly didn't meet criteria
# so I say let's remove all NAs on this variable to be safe

smarvus_data <- smarvus_data |> dplyr::filter(!is.na(degree_major))

# count and exclude 'no' responses to the attention amnesty 
# (keeping NAs due to length of survey & additional checks)

n_att_amnesty <- smarvus_data |> 
  dplyr::filter(attention_amnesty == "No") |> 
  nrow()

smarvus_data <- smarvus_data |> 
  dplyr::filter(attention_amnesty != "No" | is.na(attention_amnesty))

smarvus_data |> dplyr::distinct(attention_amnesty)

# exclude failed attention checks 

# count fails & missing
n_turkey <- smarvus_data |>
  dplyr::filter(country == "Turkey") |>
  nrow()

n_stars_fail <- smarvus_data |> 
  dplyr::filter(Q7.1_24 != "1") |> 
  nrow()

n_stars_missing <- smarvus_data |> 
  dplyr::filter(is.na(Q7.1_24)) |> 
  nrow()

n_starsm_fail <- smarvus_data |> 
  dplyr::filter(Q8.1_21 != "5") |> 
  nrow()

n_starsm_missing <- smarvus_data |> 
  dplyr::filter(is.na(Q8.1_21)) |> 
  nrow()

n_trait_fail <- smarvus_data |> 
  dplyr::filter(Q9.1_22 != "1") |> 
  nrow()

n_trait_missing <- smarvus_data |> 
  dplyr::filter(is.na(Q9.1_22)) |> 
  nrow()

n_fne_fail <- smarvus_data |> 
  dplyr::filter(Q11.1_9 != "3") |> 
  nrow()

n_fne_missing <- smarvus_data |> 
  dplyr::filter(is.na(Q11.1_9)) |> 
  nrow()

n_cas_fail <- smarvus_data |> 
  dplyr::filter(Q13.1_17 != "2") |> 
  nrow()

n_cas_missing <- smarvus_data |> 
  dplyr::filter(is.na(Q13.1_17)) |> 
  nrow()

n_persist_fail <- smarvus_data |> 
  dplyr::filter(Q15.1_9 != "4") |> 
  nrow()

n_persist_missing <- smarvus_data |> 
  dplyr::filter(is.na(Q15.1_9)) |> 
  nrow()

# count and exclude all fails (keeps only cases where all attention checks were passed)
n_att_check_excluded <- smarvus_data |> 
  dplyr::filter(Q7.1_24 == "1" & Q8.1_21 == "5" & Q9.1_22 == "1" & Q11.1_9 == "3" & 
                  Q13.1_17 == "2" & Q15.1_9 == "4")

n_att_check <- nrow(smarvus_data) - nrow(n_att_check_excluded)

smarvus_data <- smarvus_data |> 
  dplyr::filter(Q7.1_24 == "1" & Q8.1_21 == "5" & Q9.1_22 == "1" & Q11.1_9 == "3" & 
                  Q13.1_17 == "2" & Q15.1_9 == "4")

##### STATS GRADES (official, numeric) #####

# how much official grade data do we have? 
n_official <- smarvus_data |>
  dplyr::filter(!is.na(grade) & grade != "Not Available") |>
  nrow()

# get rid of % signs
smarvus_data <- smarvus_data |> 
  dplyr::mutate(grade = replace(grade, stringr::str_detect(grade, "%"), ""))

# get rid of categorical grades and text
smarvus_data <- smarvus_data |>
  dplyr::mutate(uni_grade_num = as.numeric(stringr::str_extract(grade, "\\d+\\.*\\d*")))

# change Limerick grades back to NA (because it has extracted the number from the categorical grade and I cba to 
# learn more bloody regex-type stingr stuff and we don't need Limerick grades in this variable anyway)
smarvus_data <- smarvus_data |> 
  dplyr::mutate(uni_grade_num = dplyr::case_when(
    university == "University of Limerick" ~ NA_integer_,
    TRUE ~ uni_grade_num
  ))

# how much numeric official grade data do we have? 
n_numeric <- smarvus_data |>
  dplyr::filter(!is.na(uni_grade_num) & uni_grade_num != "Not Available") |>
  nrow()

# convert to numeric
smarvus_data$grade <- as.numeric(smarvus_data$grade) |> round(2)

# standardise at the module level (all students that have grades for the module would have been taking
# that module at the time of the survey, so the modules will be internally consistent)
smarvus_data <- smarvus_data |> 
  dplyr::group_by(grade_module_name) |>
  dplyr::mutate(uni_grade_num_z = scale(uni_grade_num))

smarvus_data$uni_grade_num_z <- as.vector(smarvus_data$uni_grade_num_z)

##### STATS GRADES (official, categorical) #####

# get list of different categories
smarvus_data |> 
  dplyr::distinct(grade) |> 
  dplyr::arrange(grade)

# create new var with cat grades only
smarvus_data <- smarvus_data |>
  dplyr::mutate(uni_grade_cat = dplyr::case_when(
    grade == "A" ~ grade,
    grade == "A+" ~ grade,
    grade == "A+, A+" ~ grade,
    grade == "A, C+" ~ grade,
    grade == "A1" ~ grade,
    grade == "A2" ~ grade,
    grade == "B" ~ grade,
    grade == "B+" ~ grade,
    grade == "B+, D+" ~ grade,
    grade == "B1" ~ grade,
    grade == "B2" ~ grade,
    grade == "B3" ~ grade,
    grade == "C" ~ grade,
    grade == "C+" ~ grade,
    grade == "C+, A" ~ grade,
    grade == "C+, C" ~ grade,
    grade == "C1" ~ grade,
    grade == "C2" ~ grade,
    grade == "C+, A" ~ grade,
    grade == "C+, C" ~ grade,
    grade == "D+" ~ grade,
    grade == "E" ~ grade,
    grade == "F" ~ grade,
  ))

##### STATS GRADES (self-reported, numeric) #####

### Sky cleaned this data and created a mean of the grades (stats_edu_mean_grade) which isn't in the main data
### but which we're using as we know it has been carefully checked for weird responses

# how much sr grade data do we have? 
smarvus_data |>
  dplyr::filter(!is.na(stats_edu_mean_grade)) |>
  nrow()

# convert to numeric
smarvus_data$stats_edu_mean_grade <- as.numeric(smarvus_data$stats_edu_mean_grade) |> round(2)

# standardise
smarvus_data <- smarvus_data |> 
  dplyr::group_by(university, degree_year, stats_edu_grade_scale) |>
  dplyr::mutate(sr_grade_num_z = scale(stats_edu_mean_grade))

smarvus_data$sr_grade_num_z <- as.vector(smarvus_data$sr_grade_num_z)

##### CREATE AND SAVE SPLIT SAMPLES #####

# NOTE: If we're going to split the pooled sample, we're going to have to NOT exclude block-wise, 
# so we'll use the block 2 sample for the efa as well as the later analyses

# separate efa sample
set.seed(250523)
efa_data <- smarvus_data[sample(nrow(smarvus_data), 1000), ]

# store the rest in another tibble
core_data <- smarvus_data |>
  dplyr::filter(!unique_id %in% efa_data$unique_id)

#readr::write_csv(smarvus_data, here::here("data/full_data.csv"))
#readr::write_csv(efa_data, here::here("data/efa_data.csv"))
#readr::write_csv(core_data, here::here("data/core_data.csv"))
