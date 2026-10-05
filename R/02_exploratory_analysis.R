# ==============================================================================
# 02_exploratory_analysis.R
# Marketing Mix Model - exploratory analysis
#
# Run 01_data_preparation.R first.
# ==============================================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)

# ------------------------------------------------------------------------------
# 1. Weekly sales
# ------------------------------------------------------------------------------

summary(df$Vol_Sales)

ggplot(df, aes(x = Week_Ending, y = Vol_Sales)) +
  geom_line() +
  labs(title = "Weekly Sales", x = "Date", y = "Sales Volume") +
  theme_minimal()

ggplot(df, aes(x = Vol_Sales)) +
  geom_histogram(bins = 30, fill = "darkgrey", color = "black") +
  labs(title = "Distribution of Weekly Sales", x = "Weekly Sales", y = "Frequency") +
  theme_minimal()

# ------------------------------------------------------------------------------
# 2. Baseline variables
# ------------------------------------------------------------------------------

baseline_vars <- c("NEW_DATA_NMR", "Base_price", "Promo_Intensity", "seasonal_index_prophet")

baseline_statistics <- sapply(df[, baseline_vars], function(x) {
  c(
    Mean = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
})

print(baseline_statistics)

# Check whether seasonality alone explains an excessive amount of sales.
controllo_stag <- lm(Vol_Sales ~ seasonal_index_prophet, data = df)
summary(controllo_stag)

# ------------------------------------------------------------------------------
# 3. Sales outlier check
# ------------------------------------------------------------------------------

Q1 <- quantile(df$Vol_Sales, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Vol_Sales, 0.75, na.rm = TRUE)
IQR_sales <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_sales
upper_bound <- Q3 + 1.5 * IQR_sales

outliers <- df$Vol_Sales[df$Vol_Sales < lower_bound | df$Vol_Sales > upper_bound]
cat("Number of sales outliers:", length(outliers), "\n")

# ------------------------------------------------------------------------------
# 4. Correlation analysis
# ------------------------------------------------------------------------------

correlation_vars <- c(
  "Vol_Sales", "NEW_DATA_NMR", "Base_price", "Promo_Intensity",
  "seasonal_index_prophet", "GRP_TV_target", "GRP_TV_related_1",
  "GRP_TV_related_2", "imp_display", "imp_video_social",
  "imp_meta_youtube", "search", "competition"
)

cor_matrix <- cor(df[, correlation_vars], use = "pairwise.complete.obs")
print(cor_matrix)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  order = "original",
  tl.col = "black",
  tl.srt = 45,
  addCoef.col = "black",
  number.cex = 0.7,
  diag = FALSE
)

upper_tri <- upper.tri(cor_matrix)
cor_matrix_upper <- cor_matrix
cor_matrix_upper[!upper_tri] <- NA

significant_cors <- na.omit(as.data.frame(as.table(cor_matrix_upper)))
significant_cors <- significant_cors[abs(significant_cors$Freq) > 0.5, ]
significant_cors <- significant_cors[order(abs(significant_cors$Freq), decreasing = TRUE), ]
significant_cors$Freq <- round(significant_cors$Freq, 3)

print(significant_cors, row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. Media activity overview
# ------------------------------------------------------------------------------

media_raw <- df %>%
  select(
    Week_Ending, GRP_TV_target, GRP_TV_related_1, GRP_TV_related_2,
    imp_display, imp_video_social, imp_meta_youtube, search, competition
  ) %>%
  pivot_longer(cols = -Week_Ending, names_to = "Channel", values_to = "Activity")

ggplot(media_raw, aes(x = Week_Ending, y = Activity)) +
  geom_line() +
  facet_wrap(~ Channel, scales = "free_y", ncol = 1) +
  labs(title = "Media Activity Over Time", x = "Date", y = "Activity") +
  theme_minimal()
