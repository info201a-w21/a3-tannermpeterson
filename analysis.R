# Load incarceration data
# incarceration_trends_jail_jurisdiction <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends_jail_jurisdiction.csv")
incarceration_trends <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

# Load tidyverse
library("tidyverse")

# Value of interest: Ratio of rate of incarcerations of black people
# to rate of incarcerations of white people
county_trends_with_btw_ratio <- incarceration_trends %>%
  mutate(
    btw_ratio = (
      (black_prison_pop / black_pop_15to64)
      / (white_prison_pop / white_pop_15to64)
    )
  )

# 1) Average ratio across all counties
avg_btw_ratios <- county_trends_with_btw_ratio[
    is.finite(county_trends_with_btw_ratio$btw_ratio),
  ] %>%
  group_by(year) %>%
  summarize(avg_btw_ratio = mean(btw_ratio, na.rm = TRUE))

# 2) Highest ratio in 2016
highest_btw_ratio_2016 <- county_trends_with_btw_ratio[
    is.finite(county_trends_with_btw_ratio$btw_ratio),
  ] %>%
  filter(year == 2016) %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(btw_ratio)

# 3) Highest ratio in 2016
highest_btw_ratio_2006 <- county_trends_with_btw_ratio[
    is.finite(county_trends_with_btw_ratio$btw_ratio),
  ] %>%
  filter(year == 2006) %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(btw_ratio)

# 4) Which county/state had highest ratio in 2016
state_with_highest_btw_ratio <- county_trends_with_btw_ratio[
    is.finite(county_trends_with_btw_ratio$btw_ratio),
  ] %>%
  filter(btw_ratio == max(btw_ratio, na.rm = TRUE)) %>%
  pull(state)
county_with_highest_btw_ratio <- county_trends_with_btw_ratio[
    is.finite(county_trends_with_btw_ratio$btw_ratio),
  ] %>%
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


# Load ggplot2
library("ggplot2")

# Chart 1: Rate of incarceration by race over time
avg_rates_over_time <- incarceration_trends %>%
  group_by(year) %>%
  filter(year >= 1990 && year <= 2016) %>%
  summarize(
    avg_white_rate = mean(white_prison_pop_rate, na.rm = TRUE),
    avg_black_rate = mean(black_prison_pop_rate, na.rm = TRUE),
    avg_latinx_rate = mean(latinx_prison_pop_rate, na.rm = TRUE),
    avg_aapi_rate = mean(aapi_prison_pop_rate, na.rm = TRUE),
    avg_native_rate = mean(native_prison_pop_rate, na.rm = TRUE),
  )
rate_over_time <- ggplot(data = avg_rates_over_time) +
  geom_line(mapping = aes(x = year, y = avg_white_rate, color = "White")) +
  geom_line(mapping = aes(x = year, y = avg_black_rate, color = "Black")) +
  geom_line(mapping = aes(x = year, y = avg_latinx_rate, color = "Latinx")) +
  geom_line(mapping = aes(x = year, y = avg_aapi_rate, color = "AAPI")) +
  geom_line(mapping = aes(x = year, y = avg_native_rate, color = "Native")) +
  labs(
    title = "Average Incarcerated Population Rates by Race Over Time",
    x = "Year",
    y = "Average Incarcerated Population Rate (age 15-64) per 100,000",
    color = "Race"
  )


# Chart 2: Comparing incarceration rates of black population vs. white population
b_vs_w_pop_rate <- ggplot(data = avg_rates_over_time) +
  geom_point(
    mapping = aes(
      x = avg_white_rate,
      y = avg_black_rate,
      color = "Black/White Ratio"
    )
  ) +
  geom_abline(mapping = aes(intercept = 0, slope = 1, color = "1:1 ratio")) +
  ylim(0, 4000) +
  xlim(0, 600) +
  labs(
    title = "Comparison of Black vs. White Incarcerated Population Rates",
    x = "Average Incarcerated White Population Rate (age 15-64) per 100,000",
    y = "Average Incarcerated Black Population Rate (age 15-64) per 100,000",
    color = NULL
  )


# Map: Black Population Incarceration Rate by County
library("maps")
library("mapproj")
library("patchwork")

blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
  )

wa_counties_2016 <- incarceration_trends %>%
  filter(state == "WA") %>%
  filter(year == "2016")
county_shapes <- map_data("county") %>%
  filter(region == "washington") %>%
  mutate(county_name = paste0(str_to_title(subregion), " County"))
map_data <- county_shapes %>%
  left_join(wa_counties_2016, by = "county_name")
black_incarceration_rate_map <- ggplot(map_data) +
  geom_polygon(
    mapping = aes(
      x = long,
      y = lat,
      group = group,
      fill = black_prison_pop_rate
    ),
    color = "gray",
    size = 0.3
  ) +
  coord_map() +
  scale_fill_continuous(
    limits = c(0, max(map_data$black_prison_pop_rate)),
    na.value = "white",
    low = "yellow",
    high = "red"
  ) +
  blank_theme +
  ggtitle("Incarceration Rate of Black Population in WA by County")
