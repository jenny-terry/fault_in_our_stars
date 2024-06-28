library(magrittr)

# read in efa data (note that the parsing issues are only to do with data types, which can be fixed as required)
efa_data <- here::here("data/efa_data.csv")|> readr::read_csv()

# convert England, N. Ireland, and Scotland to UK
efa_data <- efa_data |>
  dplyr::mutate(country = dplyr::case_when(
    country == "England" ~ "United Kingdom",
    country == "Northern Ireland" ~ "United Kingdom",
    country == "Scotland" ~ "United Kingdom",
    TRUE ~ country
  ))

# count no. of universities
n_universities <- efa_data %>% dplyr::distinct(university) %>% dplyr::summarize(count = dplyr::n())

# count no. of countries
n_countries <- efa_data %>% dplyr::distinct(country) %>% dplyr::summarize(count = dplyr::n())

# count no. of languages
n_languages <- efa_data %>% dplyr::distinct(language) %>% dplyr::summarize(count = dplyr::n())

# count no. of participants
n_final <- nrow(efa_data)

### Incentives
summary_incentives <- efa_data %>%
  dplyr::group_by(incentive) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(percent))
summary_incentives

### Degree Year

summary_degree_year <- efa_data %>%
  dplyr::group_by(degree_year) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(percent))
summary_degree_year

### Degree Major
summary_degree_major <- efa_data %>%
  dplyr::group_by(degree_major) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(percent))
summary_degree_major

### Age
summary_age <- efa_data %>%
  dplyr::group_by(age) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(age))
summary_age

### Gender
summary_gender <- efa_data %>%
  dplyr::group_by(gender) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(percent)) 
summary_gender

### SpLDs
efa_data %<>%
  dplyr::mutate(spld = dplyr::case_when(
    stringr::str_detect(spld, pattern = "I do not") ~ "None",
    stringr::str_detect(spld, pattern = "Not an") ~ "None",
    TRUE ~ spld
  ))

efa_data %<>%
  dplyr::mutate(spld = dplyr::case_when(
    stringr::str_detect(spld, pattern = ",") ~ "Multiple",
    TRUE ~ spld
  ))

efa_data %<>%
  dplyr::mutate(spld = dplyr::case_when(
    stringr::str_detect(spld, pattern = "Other") ~ "Other",
    TRUE ~ spld
  ))

summary_spld <- efa_data %>%
  dplyr::group_by(spld) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(percent = formattable::percent(n / sum(n))) %>% 
  dplyr::arrange(desc(percent)) 
summary_spld

### Completion Time
efa_data$duration <- as.numeric(efa_data$duration)

efa_data %>%
  dplyr::summarise(
    mean = mean(duration, na.rm = T),
    median = median(duration, na.rm = T),
    sd = sd(duration, na.rm = T),
    min = min(duration, na.rm = T),
    max = max(duration, na.rm = T)) 

duration_mean <- mean(efa_data$duration)/60
duration_sd <- sd(efa_data$duration)/60
duration_median <- median(efa_data$duration)/60

### Countries Table (original code credit: Tamas Nagy)
library(dplyr)
library(countrycode)
library(gt)

sanx_uni <-
  efa_data %>% 
  count(country, language, university) %>% 
  mutate(continent = countrycode(country, "country.name", "continent")) %>% 
  arrange(continent, country, -n)

country_labels <-    
  efa_data %>% 
  count(country) %>%
  transmute(country, 
            country_label = paste0(country, " (N = ", n, ")"))

country_table <- sanx_uni %>% 
  left_join(country_labels, by = "country") %>% 
  arrange(country_label, -n) %>% 
  group_by(country_label) %>%
  gt() %>% 
  tab_stubhead("Country") %>%
  fmt_integer(n) %>% 
  cols_hide(c("country", "continent")) %>% 
  cols_label(language = "Language",
             university = "University",
             n = "N") %>% 
  grand_summary_rows(columns = n, fns = list(`All participants` = ~sum(.)),
                     missing_text = "",
                     formatter = fmt_integer) %>% 
  tab_options(column_labels.font.weight = "bold",
              column_labels.background.color = "grey30",
              row_group.background.color = "lightgrey", 
              row_group.font.weight = "bold", 
              grand_summary_row.background.color = "grey30",
              row_group.padding = 0,
              summary_row.padding = 0,
              grand_summary_row.padding = 0,
              data_row.padding = 0,
              column_labels.padding = 0,
              table_body.hlines.width = 0,
              stub.border.width = 0,
              table.border.top.color = "black",
              table.border.bottom.color = "black")
