##################################################################

#' Code related to Enisse Kharroubi & Marius Koechlin 2025: Labour market flows and the Phillips curve
#' Code author: Marius Koechlin
#' 
#' This code file updates the labour market momentum measure introduced in the paper mentioned above.
#' 
#' Last updated: 17.08.2026



# Set-up ------------------------------------------------------------------
# Delete all variables
rm(list = ls())

# Set language to English
Sys.setenv(LANG = "en")

# Load packages
library("dplyr")         # Code structure
library("tibble")
library("tidyr")         # Data treatment
library("ggplot2")       # Graphs
library("readxl")        # To load excel files into R
library("blsR")          # To load data from BLS
library("fredr")         # To load data from FRED
library("lubridate")     # Work with dates
library("zoo")           # Work with dates

# Set your API key (if used) (can be set-up here: https://fred.stlouisfed.org/docs/api/api_key.html
fredr_set_key(
  # REPLACE WITH YOUR FRED API KEY !
  frd_api_key <- Sys.getenv("FRED_API")
)

# Load data ---------------------------------------------------------------

# Define date
startdate <- as.Date("1990-01-01")
enddate <- as.Date(today())

## Labour Market Flows
#' The data can be downloaded here: https://fred.stlouisfed.org/release/tables?rid=334&eid=1147#snid=1189
#' Or an API can be used (either from FRED or BLS). 

# Data on the data series names
bls_lfs_id <- read.csv("rawdata/bls_lfs_id.csv")

# Load the data using the FRED API
# Prepare dataframe
lfs_data <- tibble(date = numeric(), value = numeric(), series_id = character())

# Loop over each of the nine transitions
for (i in bls_lfs_id$series_id) {
  flow_i <- 
    fredr::fredr(i, observation_start = startdate, observation_end = enddate, frequency = "m", units = "lin") %>% 
    dplyr::select(date, value) %>% mutate(series_id = i)
  lfs_data <- rbind(lfs_data, flow_i)
}

# Clean data
lfs_data <- lfs_data %>%
  rename(flow_ths_pers = value)

# Join data with series names
lfs_data <- lfs_data %>%
  left_join(bls_lfs_id, join_by("series_id"))


## Additional data, such as stock unemployment rate
#' The data can be downloaded from the BLS website: https://www.bls.gov/cps/lfcharacteristics.htm
#' Or an API can be used (either from FRED or BLS).

# Data on the data series names
bls_lm_id <- read.csv("rawdata/bls_lm_id.csv")

# Load the data using the FRED API
lm_data <- as_tibble(
  rbind(
    # Unemployment Level (UNEMPLOY) [BLS: LNS13000000]
    fredr::fredr("UNEMPLOY", observation_start = startdate, observation_end = enddate, frequency = "m", units = "lin") %>%
      dplyr::select(date, value) %>% mutate(series_id = "LNS13000000"),
    # Unemployment Rate (UNRATE) [BLS: LNS14000000]
    fredr::fredr("UNRATE", observation_start = startdate, observation_end = enddate, frequency = "m", units = "lin") %>%
      dplyr::select(date, value) %>% mutate(series_id = "LNS14000000"),
    # Population Level (CNP16OV) [BLS: LNU00000000]
    fredr::fredr("CNP16OV", observation_start = startdate, observation_end = enddate, frequency = "m", units = "lin") %>%
      dplyr::select(date, value) %>% mutate(series_id = "LNU00000000"),
    # Consumer Price Index for All Urban Consumers: All Items Less Food and Energy in U.S. City Average (CPILFESL) [BLS: CUSR0000SA0L1E]
    fredr::fredr("CPILFESL", observation_start = startdate, observation_end = enddate, frequency = "m", units = "lin") %>%
      dplyr::select(date, value) %>% mutate(series_id = "CUSR0000SA0L1E")
  )
)

# Clean data
lm_data <- lm_data %>%
  mutate(date = as.Date(date),
         value = as.numeric(value))

# Join data with series names
lm_data <- lm_data %>%
  left_join(bls_lm_id, join_by("series_id"))

# Add name for CPI
lm_data <- lm_data %>%
  mutate(series_name_short = ifelse(series_id == "CUSR0000SA0L1E", "cpi_core_idx", series_name_short))


# Data treatment ----------------------------------------------------------
## Labour Market Flows
# Prepare the flows and calculate the monthly transition probabilities
lfs_data <- lfs_data %>%
  group_by(date, t0) %>%
  mutate(total_flow = sum(flow_ths_pers)) %>% ungroup() %>%
  mutate(trans_prob = flow_ths_pers / total_flow) %>%
  dplyr::select(date, t0, t1, transition, flow_ths_pers, total_flow, trans_prob)

## Additional data
# Adjust dataset & calculate the unemployment rate as a share of the working age population
lm_data <- lm_data %>%
  dplyr::select(date, series_name_short, value) %>%
  pivot_wider(names_from = series_name_short, values_from = value) %>%
  mutate(u_rate = unemployed / population * 100) %>%
  # Calculate the core CPI inflation rate (yoy)
  mutate(cpi_core_yoy = (log(cpi_core_idx) - log(dplyr::lag(cpi_core_idx, 12))) * 100)



# Calculate the flow-based unemployment rate ------------------------------
# Prepare dataframe to store results
flow_u_data <- tibble(date = numeric(), flow_u = numeric())

# Start loop
for (date_t in unique(lfs_data$date)) {
  
  # Prepare p matrix
  p_matrix <- lfs_data %>% 
    dplyr::select(c(date, transition, t0, t1, trans_prob)) %>%
    filter(date == date_t) %>% dplyr::select(-c(date, transition)) %>%
    pivot_wider(names_from = t1, values_from = trans_prob) %>% 
    column_to_rownames(var = "t0") %>% as.matrix()
  
  # Calculate q matrix
  q_matrix <- matrix(
    c(p_matrix["E", "E"] - p_matrix["I", "E"],
      p_matrix["E", "U"] - p_matrix["I", "U"],
      p_matrix["U", "E"] - p_matrix["I", "E"],
      p_matrix["U", "U"] - p_matrix["I", "U"]),
    nrow = 2, ncol = 2, byrow = TRUE)
  rownames(q_matrix) <- c("E", "U")
  colnames(q_matrix) <- c("E", "U")
  
  # Calculate the flow based unemployment rate
  flow_u <- ((1 - q_matrix["E", "E"]) * p_matrix["I", "U"] + p_matrix["I", "E"] * q_matrix["E", "U"]) /
    ((1 - q_matrix["E", "E"]) * (1 - q_matrix["U", "U"]) -  q_matrix["U", "E"] * q_matrix["E", "U"])
  
  # Add to main dataframe
  flow_u_data <- bind_rows(flow_u_data, tibble(date = date_t, flow_u = flow_u))
  
}

# Final adjustments
flow_u_data <- flow_u_data %>%
  mutate(date = as.Date(date),
         flow_u = flow_u * 100)


# Combine the data & calculate the LMM ------------------------------------
# Combine data
final_data <- 
  lm_data %>% dplyr::select(date, u_rate, cpi_core_yoy) %>%
  left_join(flow_u_data, by = "date") 

# Calculate the labour market momentum
final_data <- final_data %>%
  mutate(lmm = u_rate - flow_u) %>%
  # Calculate the 6-month moving average
  mutate(across(c(u_rate, flow_u, lmm), ~ rollmean(., 6, fill = NA, align = "right", na.rm = TRUE), .names = "{col}_ma6"))


# Save the dataset --------------------------------------------------------
# Save as CSV
write.csv(final_data %>% dplyr::select(date, lmm, lmm_ma6) %>% rename(KK_lmm = lmm, KK_lmm_ma6 = lmm_ma6) %>% drop_na(),
          file = "output/Kharroubi_Koechlin_lmm_data.csv", row.names = FALSE)


# Some analysis -----------------------------------------------------------

# Plot the stock-based & flow-based rate
final_data %>%
  ggplot(aes(x = date)) +
  geom_rect(xmin = as.Date("1990-08-01"), xmax = as.Date("1991-03-31"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2008-01-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_line(aes(y = u_rate_ma6, color = "Stock-based unemployment rate")) +
  geom_line(aes(y = flow_u_ma6, color = "Flow-based unemployment rate")) +
  scale_color_manual(values = c("Stock-based unemployment rate" = "#58078c", 
                                 "Flow-based unemployment rate" = "#aa332f"),
                     name = "") +
  labs(title = "Flow-, and stock-based unemployment", x = "", y = "Percentage") +
  scale_x_date(breaks = seq(as.Date("1990-01-01"), max(as.Date("2025-12-01"), na.rm = TRUE), by = "5 years"), date_labels = "%Y") +
  theme_minimal(base_family = "Palatino", base_size = 20) +
  theme(legend.position = "bottom", legend.margin=margin(-5,0,5,0), legend.box.spacing = unit(1, "pt"))
# Save the plot
ggsave("output/stock_flow_u_rate.png", width = 22, height = 12, units = "cm")

# Plot the unemployment gap
final_data %>%
  ggplot(aes(x = date)) +
  geom_rect(xmin = as.Date("1990-08-01"), xmax = as.Date("1991-03-31"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2008-01-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_line(aes(y = lmm_ma6)) +
  # Fill negative and positive area
  geom_ribbon(aes(ymin = pmin((lmm_ma6), 0), ymax = 0), fill = "#aa332f", alpha = 0.3) +
  geom_ribbon(aes(ymin = 0, ymax = pmax((lmm_ma6), 0)), fill = "green3", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
  labs(title = "Labour market momentum", x = "", y = "Percentage points") +
  scale_x_date(breaks = seq(as.Date("1990-01-01"), max(as.Date("2025-12-01"), na.rm = TRUE), by = "5 years"), date_labels = "%Y") +
  theme_minimal(base_family = "Palatino", base_size = 20) +
  theme(legend.position = "bottom", legend.margin=margin(-5,0,5,0), legend.box.spacing = unit(1, "pt"))
ggsave("output/lmm.png", width = 22, height = 12, units = "cm")


final_data %>%
  ggplot(aes(x = date)) +
  geom_rect(xmin = as.Date("1990-08-01"), xmax = as.Date("1991-03-31"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2001-03-01"), xmax = as.Date("2001-11-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2008-01-01"), xmax = as.Date("2009-06-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_rect(xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-30"), ymin = -Inf, ymax = Inf, fill = "gray90", alpha = 0.02) +
  geom_line(aes(y = dplyr::lead(cpi_core_yoy, 12), color = "1-year ahead Core CPI (lhs)")) +
  geom_line(aes(y = (lmm_ma6 * 4) + 2, color = "Labour market momentum (rhs)")) +
  scale_color_manual(name = "", values = c("Labour market momentum (rhs)" = "#aa332f", "1-year ahead Core CPI (lhs)" = "#58078c")) +
  labs(x = "", y = "Percentage", title = "Core CPI inflation and the LMM") +  
  scale_x_date(breaks = seq(as.Date("1990-01-01"), max(as.Date("2025-12-01"), na.rm = TRUE), by = "5 years"), date_labels = "%Y") +
  scale_y_continuous(breaks = scales::breaks_extended(n = 6),
                     sec.axis = sec_axis(~., name = "Percentage points", labels = function(x) (x - 2)/4,
                                         breaks = function(lims) unique(c(0 * 4 + 2, scales::breaks_extended(n = 6)(lims))) )) +
  theme_minimal(base_family = "Palatino", base_size = 20) +
  theme(legend.position = "bottom", legend.margin=margin(-5,0,5,0), legend.box.spacing = unit(1, "pt"))
ggsave("output/core_cpi_lmm.png", width = 22, height = 12, units = "cm")


# Summary statistics of the LMM
final_data %>%
  summarise(mean_gap = mean(lmm_ma6, na.rm = TRUE),
            sd_gap = sd(lmm_ma6, na.rm = TRUE),
            min_gap = min(lmm_ma6, na.rm = TRUE),
            max_gap = max(lmm_ma6, na.rm = TRUE),
            median_gap = median(lmm_ma6, na.rm = TRUE))

# Development in the last three month of the LMM
final_data %>%
  filter(date >= max(date) - months(3)) %>%
  summarise(mean_gap = mean(lmm_ma6, na.rm = TRUE),
            sd_gap = sd(lmm_ma6, na.rm = TRUE),
            min_gap = min(lmm_ma6, na.rm = TRUE),
            max_gap = max(lmm_ma6, na.rm = TRUE),
            median_gap = median(lmm_ma6, na.rm = TRUE))



############################### END ##############################
##################################################################
