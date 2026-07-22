library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(here)
library(broom)

# Locate the data
data_dir <- here::here("data")

# Read the JSON line list and parse dates ---------------------------------
df <- fromJSON(file.path(data_dir, "malaria", "calendar.json")) |>
  mutate(
    date_daily = as.Date(as.character(blood_draw_date), format = "%Y%m%d"),
    month      = floor_date(date_daily, "month")
  )


counts_per_day <- df |>
  count(date_daily, name = "count") |>
  arrange(date_daily)
counts_per_month <- df |>
  count(month, name = "count") |>
  arrange(month)

p_day <- ggplot(counts_per_day, aes(date_daily, count)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "date", y = "total daily counts") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_month <- ggplot(counts_per_month, aes(month, count)) +
  geom_line() + geom_point(size = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  labs(x = "date (monthly total, aligned to first of month)",
       y = "total monthly counts") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_day / p_month

# Counts per province per month -------------------------------------------
counts_prov_month <- df |>
  count(month, province_en, name = "Count") |>
  arrange(month, province_en)

# Keep provinces with more than 12 months of reported cases 
selected_provinces <- counts_prov_month |>
  add_count(province_en, name = "n_months") |>
  filter(n_months > 12) |>
  distinct(province_en) |>
  pull(province_en)
glue::glue("Selected provinces: {length(selected_provinces)}")



# Facet titles: how many of the possible months each province reports ---------
n_possible <- n_distinct(counts_prov_month$month)     # total months in the data (18)
prov_counts <- counts_prov_month |>
  filter(province_en %in% selected_provinces) |>
  group_by(province_en) |>
  summarise(n_months = n_distinct(month), .groups = "drop")
prov_lab <- prov_counts |>
  transmute(
    province_en,
    label = paste0(province_en, " (", n_months, "/", n_possible, ")")
  ) |>
  tibble::deframe()

counts_prov_month |>
  filter(province_en %in% selected_provinces) |>
  ggplot(aes(month, Count)) +
  geom_line() + geom_point(size = 0.6) +
  facet_wrap(~ province_en, scales = "free_y",
             labeller = labeller(province_en = prov_lab)) +
  labs(x = "date", y = "monthly count") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))


### Finer spatial scale

# Counts per district per month (finer spatial scale) ---------------------
counts_dist_month <- df |>
  count(month, district_en, name = "Count")

# Would a finer (district) scale work? 
n_months_total <- n_distinct(df$month)                  # 18 possible months
dist_months <- counts_dist_month |>
  group_by(district_en) |>
  summarise(n_months = n_distinct(month), .groups = "drop")

glue::glue("Districts in total: {nrow(dist_months)}")
glue::glue("Districts with >12 months of reported cases: {sum(dist_months$n_months > 12)}")
glue::glue("Provinces with >12 months (from above): {length(selected_provinces)}")

# Share of all reported cases falling in the qualifying districts vs provinces
ok_dist <- dist_months |>
  filter(n_months > 12) |>
  pull(district_en)
glue::glue("The {length(ok_dist)} qualifying districts hold {100 * mean(df$district_en %in% ok_dist)}% of all reported cases",)
glue::glue("The {length(selected_provinces)} qualifying provinces hold {round(100*mean(df$province_en %in% selected_provinces), digits = 1)}% of all reported cases")

# Months of reported cases per province and per district
prov_months <- counts_prov_month |>
  group_by(province_en) |>
  summarise(n_months = n_distinct(month), .groups = "drop")

months_bar <- function(d, unit_label) {
  ggplot(d, aes(n_months)) +
    geom_bar(fill = "steelblue") +
    geom_vline(xintercept = 12.5, linetype = "dashed", colour = "red") +
    scale_x_continuous(
      breaks = seq_len(n_months_total),
      limits = c(0.5, n_months_total + 0.5)
    ) +
    labs(y = paste0("number of ", unit_label)) +
    theme_bw()
}

p_prov_freq <- months_bar(prov_months, "provinces") +
  labs(x = NULL, title = "Provinces by number of months with reported cases")
p_dist_freq <- months_bar(dist_months, "districts") +
  labs(x = paste0("months with reported cases (out of ", n_months_total, ")"),
       title = "Districts by number of months with reported cases")

p_prov_freq / p_dist_freq