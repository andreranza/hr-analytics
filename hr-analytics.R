library(tidyverse)

# Absences ----------------------------------------------------------------

absences_df <- read_csv("data/absences_attedances.csv")

# compute number of days in each month for a given absence
absences_df <- absences_df %>% 
  mutate(n_days = map2(start, end, ~ seq(.x, .y, by = "day"))) %>% 
  mutate(
    n_days = map(
      n_days,
      function(x) {
        str_c(
          lubridate::year(x), 
          str_pad(lubridate::month(x), pad = "0", width = 2, side = "left"),
          sep = "-"
        )
      }
    )
  ) %>% 
  mutate(n_days = map(n_days, as_tibble_col)) %>% 
  mutate(n_days = map(n_days, count, value)) %>% 
  unnest(n_days) %>% 
  rename(month = value, n_days = n)

# Organization ------------------------------------------------------------

org_df <- read_csv("data/org_assignment.csv")

hr_df <- left_join(absences_df, org_df, by = "staff_id")

# Task 1 ------------------------------------------------------------------

task1 <- hr_df %>% 
  filter(abs_attend_type == "EducationalLeave") %>% 
  group_by(staff_id) %>% 
  summarise(edu_days = sum(n_days), salary_band = salary_band, contract_type = contract_type) %>% 
  filter(edu_days > 5) %>% 
  ungroup()

task1 %>% 
  summarise(n_distinct(staff_id))

# Task 2 ------------------------------------------------------------------

task2 <- hr_df %>% 
  filter(abs_attend_type == "AnnualLeave") %>% 
  group_by(contract_type) %>% 
  summarise(median_leave_days = median(n_days, na.rm = T))

hr_df %>% 
  filter(abs_attend_type == "AnnualLeave") %>% 
  group_by(staff_id, start, end, contract_type) %>% 
  summarise(n_days = sum(n_days)) %>% 
  ggplot(aes(x = contract_type, y = n_days, group = contract_type)) +
  geom_boxplot() +
  labs(title = "Median number of annual leave days per contract type") +
  theme(plot.title = element_text(hjust = 0.5))

# Task 3 ------------------------------------------------------------------

# find total number of employee per business area
buss_area <- hr_df %>% 
  group_by(area_id) %>% 
  summarise(n_emp = n_distinct(staff_id))

# find total number of days of comp leave per employee,
# subset those who have more than 10 and count how many thay are
compensatory <- hr_df %>% 
  filter(abs_attend_type == "CompensatoryLeave") %>% 
  group_by(area_id, staff_id) %>% 
  summarise(tot_comp_days = sum(n_days)) %>% 
  ungroup()

compensatory_10 <- compensatory %>% 
  filter(tot_comp_days > 10) %>% 
  group_by(area_id) %>% 
  summarise(n = n_distinct(staff_id))

# top 3 business area with highest percentage of employee that more than 10 days of
# compensatory leave
left_join(buss_area, compensatory_10, by = "area_id") %>% 
  mutate(n = replace_na(n, 0)) %>% 
  mutate(perc_10 = (n/n_emp)*100) %>% 
  arrange(desc(perc_10)) %>% 
  slice(1:3)
  
# Task 4 ------------------------------------------------------------------

hr_df %>% 
  filter(abs_attend_type == "Teleworking") %>% 
  ggplot(aes(x = month, y = n_days, group = month)) +
  geom_boxplot() +
  labs(title = "Average number of Teleworking days across month") +
  theme(plot.title = element_text(hjust = 0.5))

hr_df %>% 
  filter(abs_attend_type == "Teleworking") %>% 
  group_by(month) %>% 
  summarise(n_days = mean(n_days)) %>% 
  ggplot(aes(x = month, y = n_days, group = month)) +
  geom_col() +
  labs(title = "Average number of Teleworking days across month", y = "Mean of days") +
  theme(plot.title = element_text(hjust = 0.5))
