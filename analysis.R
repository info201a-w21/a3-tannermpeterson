# Load incarceration data
# incarceration_trends_jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Load tidyverse
library("tidyverse")

# Value of interest: Ratio of incarcerations of black people
# to incarcerations of to white people
county_trends_with_btw_ratio <- incarceration_trends %>%
  mutate(btw_ratio = (black_prison_pop / black_pop_15to64) / (white_prison_pop / white_pop_15to64))

# 1) Average ratio across all counties
avg_btw_ratios <- county_trends_with_btw_ratio[is.finite(county_trends_with_btw_ratio$btw_ratio), ] %>%
  group_by(year) %>%
  summarize(avg_btw_ratio = mean(btw_ratio, na.rm = TRUE))

# 2) Highest ratio in 2016
highest_btw_ratio_2016 <- county_trends_with_btw_ratio[is.finite(county_trends_with_btw_ratio$btw_ratio), ] %>%
  filter(year == 2016) %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(btw_ratio)

# 3) Highest ratio in 2016
highest_btw_ratio_2006 <- county_trends_with_btw_ratio[is.finite(county_trends_with_btw_ratio$btw_ratio), ] %>%
  filter(year == 2006) %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(btw_ratio)

# 4) Which county/state had highest ratio in 2016
state_with_highest_btw_ratio <- county_trends_with_btw_ratio[is.finite(county_trends_with_btw_ratio$btw_ratio), ] %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(state)
county_with_highest_btw_ratio <- county_trends_with_btw_ratio[is.finite(county_trends_with_btw_ratio$btw_ratio), ] %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(county_name) %>%
  paste0(", ", state_with_highest_btw_ratio)

# 5) Change in average ratio over most recent 10 years across all counties
avg_btw_ratios_2006 <- avg_btw_ratios %>%
  filter(year == 2006) %>%
  pull(avg_btw_ratio)
avg_btw_ratios_2016 <- avg_btw_ratios %>%
  filter(year == 2016, na.rm = TRUE) %>%
  pull(avg_btw_ratio)
change_in_btw_ratio_10_yrs <- avg_btw_ratios_2016 - avg_btw_ratios_2006

