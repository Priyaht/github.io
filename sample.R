# ------------------------------------------------------------
# 1.Install and Load required packages
# ------------------------------------------------------------

install.packages(c(
  "readODS",
  "dplyr",
  "stringr",
  "janitor",
  "tidyr",
  "ggplot2",
  "lubridate",
  "readr"
))

library(readODS)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)

# ------------------------------------------------------------
# 2. Import raw datasets from ODS files
# ------------------------------------------------------------

assists_4210_raw <- read_ods("table-4210-assists-and-bookings.ods", sheet = 3)
assists_4213_raw <- read_ods("table-4213-passenger-assists-by-station-operator.ods", sheet = 3)

# ------------------------------------------------------------
# 3. Clean column names and remove empty rows.
# ------------------------------------------------------------

assists_4210 <- assists_4210_raw %>%
  clean_names() %>%
  filter(if_any(everything(), ~ !is.na(.x) & str_trim(as.character(.x)) != ""))

assists_4213 <- assists_4213_raw %>%
  clean_names() %>%
  filter(if_any(everything(), ~ !is.na(.x) & str_trim(as.character(.x)) != ""))

# ------------------------------------------------------------
# 4.Function to extract periodic data only
# ------------------------------------------------------------

extract_periodic <- function(data_tbl) {

  data_tbl <- as_tibble(data_tbl)

  # Combine all columns in each row into a single text string.
  tagged <- data_tbl %>%
    mutate(
      row_id = row_number(),
      row_text = apply(across(everything(), as.character), 1, paste, collapse = " ") |>
        str_squish()
    )

  # Identify the first row containing periodic data
  first_period_row <- tagged %>%
    filter(str_detect(row_text, "Period\\s*\\d{1,2}")) %>%
    summarise(first_row = min(row_id)) %>%
    pull(first_row)

  # The header row is directly above the first periodic row
  header_row <- first_period_row - 1

  # Extract column names from the header row
  new_names <- data_tbl %>%
    slice(header_row) %>%
    unlist(use.names = FALSE) %>%
    as.character() %>%
    str_squish()

  # Build the cleaned periodic dataset
  out <- data_tbl %>%
    slice(first_period_row:n()) %>%
    setNames(new_names) %>%
    clean_names()

  # Ensure the first column is named "time_period"
  if (!"time_period" %in% names(out)) names(out)[1] <- "time_period"

  out %>% filter(str_detect(time_period, "Period")) #Remove the annual data for the datasets.
}

# Apply the periodic extraction function to both datasets.
assists_4210_periodic <- extract_periodic(assists_4210)
assists_4213_periodic <- extract_periodic(assists_4213)

# ------------------------------------------------------------
# 5. Create time variables (financial year, period number)
# ------------------------------------------------------------

# These variables support trend analysis and visualisation.

assists_4210_periodic <- assists_4210_periodic %>%
  mutate(
    financial_year = str_extract(time_period, "Apr\\s\\d{4}\\sto\\sMar\\s\\d{4}"),  # Extract the financial year
    period = str_extract(time_period, "Period\\s*\\d{1,2}") |> str_remove("Period") |> as.integer()
  ) %>%
  arrange(financial_year, period) %>%
  mutate(t = row_number())

assists_4213_periodic <- assists_4213_periodic %>%

  # Remove extra spaces and ensure the time_period column is treated as text
    mutate(
    time_period = str_squish(as.character(time_period)),
    financial_year = str_extract(time_period, "Apr\\s*\\d{4}\\s*to\\s*Mar\\s*\\d{4}"),
    period = str_extract(time_period, "Period\\s*\\d{1,2}") |> str_remove("Period") |> as.integer()      # remove the word "Period", and convert the result to an integer

  ) %>%
  arrange(financial_year, period) %>%
  mutate(t = row_number())

# ------------------------------------------------------------
# 6. Convert numeric columns
# ------------------------------------------------------------

# Removes commas and converts character numbers to numeric.
make_numeric <- function(x) {
  x <- str_replace_all(as.character(x), ",", "")
  suppressWarnings(as.numeric(x))
}

# Apply numeric conversion to all columns in Table 4210 while keeping time-related variables unchanged.

assists_4210_periodic <- assists_4210_periodic %>%
  mutate(across(-c(time_period, financial_year, period, t), make_numeric))

# Apply the same numeric conversion to Table 4213 , keeping the time related variables unchanged

assists_4213_periodic <- assists_4213_periodic %>%
  mutate(across(-c(time_period, financial_year, period), make_numeric))

# ------------------------------------------------------------
# 7. Trend in passenger assistance over time
# Histogram showing the distribution of passenger assistance demand across railway periods.
# ------------------------------------------------------------
ggplot(assists_4210_periodic,
       aes(x = passenger_assists)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of passenger assistance demand",
    x = "Passenger assists",
    y = "Frequency"
  ) +
  theme_minimal()


# ------------------------------------------------------------
# 8.Bar chart comparing average passenger assistance levels across rail operators.
# This visual highlights differences in accessibility demand within the rail network.
# ------------------------------------------------------------
assists_4213_long <- assists_4213_periodic %>%
  pivot_longer(
    cols = -c(time_period, financial_year, period, t),
    names_to = "operator",
    values_to = "assists"
  )

# Calculate the average number of assists provided by each operator

operator_summary <- assists_4213_long %>%
  group_by(operator) %>%
  summarise(average_assists = mean(assists, na.rm = TRUE)) %>%
  arrange(desc(average_assists))

# Create a bar chart to visually compare operators
ggplot(operator_summary,
       aes(x = reorder(operator, average_assists),
           y = average_assists)) +
  geom_col(fill = "#2C3E50") +
  coord_flip() +
  labs(
    title = "Average passenger assistance by operator",
    x = "Operator",
    y = "Average assists per period"
  ) +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------
# 9.Heatmap visualising passenger assistance demand across financial years
# and railway periods to identify temporal patterns and variations.
# ------------------------------------------------------------
ggplot(assists_4210_periodic,
       aes(x = period, y = financial_year, fill = passenger_assists)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(
    title = "Heatmap of passenger assistance demand",
    x = "Railway period",
    y = "Financial year",
    fill = "Assists"
  ) +
  theme_minimal()

