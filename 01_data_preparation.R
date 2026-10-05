# ==============================================================================
# 01_data_preparation.R
# Marketing Mix Model - data preparation
#
# Public version of the thesis code.
# The original dataset is proprietary and is not included in this repository.
# Company, brand and campaign names have been anonymized.
# ==============================================================================

library(readxl)
library(dplyr)
library(ggplot2)
library(prophet)

# ------------------------------------------------------------------------------
# 1. Load private data
# ------------------------------------------------------------------------------

data_path <- "data/private/marketing_data.xlsx"

if (!file.exists(data_path)) {
  stop("Private dataset not found. The proprietary dataset used in the thesis is not distributed with this repository.")
}

df <- read_excel(data_path, sheet = "model_data")

# ------------------------------------------------------------------------------
# 2. Clean column names
# ------------------------------------------------------------------------------

clean_names <- function(x) {
  x <- gsub(" ", "_", x)
  x <- gsub("[^[:alnum:]_]", "", x)
  x
}

names(df) <- clean_names(names(df))

# Expected anonymized variables:
# Week_Ending, Vol_Sales, NEW_DATA_NMR, Base_price, Promo_Intensity,
# GRP_TV_target, GRP_TV_related_1, GRP_TV_related_2,
# imp_display, imp_meta_youtube, imp_video_social,
# CLICK_Search_Channel_A, CLICK_Search_Channel_B, competition

# ------------------------------------------------------------------------------
# 3. Aggregate Search
# ------------------------------------------------------------------------------

df$search <- df$CLICK_Search_Channel_A + df$CLICK_Search_Channel_B

# ------------------------------------------------------------------------------
# 4. Estimate seasonality with Prophet
# ------------------------------------------------------------------------------

prophet_data <- df %>%
  select(Week_Ending, Vol_Sales) %>%
  rename(ds = Week_Ending, y = Vol_Sales)

m <- prophet(
  prophet_data,
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  seasonality.mode = "multiplicative",
  growth = "linear"
)

forecast <- predict(m, prophet_data)

yearly_seasonal <- forecast$yearly
weekly_seasonal <- forecast$weekly
seasonal_component <- yearly_seasonal + weekly_seasonal

seasonal_index <- exp(seasonal_component) / mean(exp(seasonal_component))
df$seasonal_index_prophet <- seasonal_index

print(mean(df$seasonal_index_prophet))
summary(df$seasonal_index_prophet)

prophet_plot_components(m, forecast)

mape_prophet <- mean(abs((prophet_data$y - forecast$yhat) / prophet_data$y)) * 100
rmse_prophet <- sqrt(mean((prophet_data$y - forecast$yhat)^2))

cat("Prophet MAPE:", mape_prophet, "\n")
cat("Prophet RMSE:", rmse_prophet, "\n")

ggplot(df, aes(x = Week_Ending, y = seasonal_index_prophet)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(title = "Seasonal Index (Prophet)", x = "Date", y = "Seasonal Index") +
  theme_minimal()
