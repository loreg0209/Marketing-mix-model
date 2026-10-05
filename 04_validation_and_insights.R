# ==============================================================================
# 04_validation_and_insights.R
# Final model diagnostics, bootstrap validation, contribution decomposition
# and ROI structure
#
# Run 03_nonlinear_mmm.R first.
# ==============================================================================

library(minpack.lm)
library(lmtest)
library(boot)
library(moments)
library(ggplot2)

# ==============================================================================
# 1. FINAL MODEL PERFORMANCE
# ==============================================================================

predictions <- predict(modello_nlsLM_finale, newdata = df)
actual <- df$Vol_Sales

r2 <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
mape <- mean(abs((actual - predictions) / actual)) * 100
rmse <- sqrt(mean((actual - predictions)^2))
mae <- mean(abs(actual - predictions))

cat("R2:", r2, "\n")
cat("MAPE:", mape, "\n")
cat("RMSE:", rmse, "\n")
cat("MAE:", mae, "\n")

# ==============================================================================
# 2. RESIDUAL DIAGNOSTICS
# ==============================================================================

residui <- residuals(modello_nlsLM_finale)
valori_previsti <- fitted(modello_nlsLM_finale)

plot(
  valori_previsti,
  residui,
  xlab = "Fitted Values",
  ylab = "Residuals",
  main = "Residuals vs Fitted"
)
abline(h = 0, col = "red")

qqnorm(residui)
qqline(residui)

print(shapiro.test(residui))
print(bptest(residui ~ valori_previsti))

vcov_matrix <- vcov(modello_nlsLM_finale)
cor_matrix <- cov2cor(vcov_matrix)
print(cor_matrix)

# ==============================================================================
# 3. BOOTSTRAP VALIDATION
# ==============================================================================

boot_func <- function(data, indices) {
  boot_data <- data[indices, ]

  tryCatch({
    model <- nlsLM(
      formula = formula_nlsLM_finale,
      data = boot_data,
      start = start_values_final,
      lower = lower_bounds_final,
      upper = upper_bounds_final,
      control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
    )

    predictions <- predict(model, newdata = boot_data)
    actual <- boot_data$Vol_Sales

    r2 <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)
    mape <- mean(abs((actual - predictions) / actual)) * 100
    rmse <- sqrt(mean((actual - predictions)^2))
    mae <- mean(abs(actual - predictions))

    return(c(R2 = r2, MAPE = mape, RMSE = rmse, MAE = mae))

  }, error = function(e) {
    return(c(R2 = NA, MAPE = NA, RMSE = NA, MAE = NA))
  })
}

set.seed(123)
n_boot <- 1000

cat("Running bootstrap with", n_boot, "replications...\n")

boot_results <- boot(
  data = df,
  statistic = boot_func,
  R = n_boot
)

results_summary <- function(boot_obj, metric_index, metric_name) {
  valid_results <- boot_obj$t[!is.na(boot_obj$t[, metric_index]), metric_index]

  mean_val <- mean(valid_results)
  sd_val <- sd(valid_results)
  ci <- quantile(valid_results, c(0.025, 0.975))

  cat("\n", metric_name, "\n", sep = "")
  cat("Mean:", round(mean_val, 4), "\n")
  cat("SD:", round(sd_val, 4), "\n")
  cat("95% CI:", round(ci[1], 4), "to", round(ci[2], 4), "\n")

  return(list(
    mean = mean_val,
    sd = sd_val,
    ci = ci,
    valid_results = valid_results
  ))
}

metrics <- list(
  R2 = results_summary(boot_results, 1, "R2"),
  MAPE = results_summary(boot_results, 2, "MAPE"),
  RMSE = results_summary(boot_results, 3, "RMSE"),
  MAE = results_summary(boot_results, 4, "MAE")
)

par(mfrow = c(2, 2))

for (i in 1:4) {
  metric_name <- c("R2", "MAPE", "RMSE", "MAE")[i]
  valid_results <- boot_results$t[!is.na(boot_results$t[, i]), i]

  hist(
    valid_results,
    main = paste("Bootstrap Distribution of", metric_name),
    xlab = metric_name,
    breaks = 30,
    probability = TRUE
  )

  lines(density(valid_results), col = "red", lwd = 2)
  abline(v = mean(valid_results), col = "blue", lwd = 2)
  abline(
    v = quantile(valid_results, c(0.025, 0.975)),
    col = "green",
    lty = 2,
    lwd = 2
  )
}

par(mfrow = c(1, 1))

# ==============================================================================
# 4. CONTRIBUTION DECOMPOSITION
# ==============================================================================

calcola_contributi_baseline <- function(modello, dati) {
  with(dati, {
    componente_moltiplicativa <- (seasonal_index_prophet^coef(modello)["c_Stag"]) *
      (log(Base_price)^coef(modello)["c_base_price"]) *
      (I(NEW_DATA_NMR^0.5)^coef(modello)["c_ref"])

    contributo_baseline <- componente_moltiplicativa * coef(modello)["intercetta"]
    contributo_promo <- componente_moltiplicativa * coef(modello)["c_promo"] * Promo_Intensity

    # Media terms are additive in the final model, so they are not multiplied
    # by the multiplicative baseline component.
    contributo_tv_target <- coef(modello)["c_target"] * log(TV_target_transformed + 1)
    contributo_meta_youtube <- coef(modello)["c_meta_youtube"] * meta_youtube_transformed
    contributo_search <- coef(modello)["c_search"] * search_transformed
    contributo_competitors <- coef(modello)["c_competitors"] * competitors_transformed

    contributi_totali <- c(
      Baseline = sum(contributo_baseline),
      Promo = sum(contributo_promo),
      TV = sum(contributo_tv_target),
      Meta_YouTube = sum(contributo_meta_youtube),
      Search = sum(contributo_search),
      Competitors = sum(contributo_competitors)
    )

    return(contributi_totali)
  })
}

contributi_baseline <- calcola_contributi_baseline(modello_nlsLM_finale, df)
print(contributi_baseline)

decomposizione_percentuale <- contributi_baseline / sum(contributi_baseline) * 100
print(decomposizione_percentuale)

somma_previsioni <- sum(predict(modello_nlsLM_finale, newdata = df))
somma_contributi <- sum(contributi_baseline)
differenza <- abs(somma_previsioni - somma_contributi)

cat("Sum of model predictions:", somma_previsioni, "\n")
cat("Sum of contributions:", somma_contributi, "\n")
cat("Absolute difference:", differenza, "\n")

df_contributi <- data.frame(
  Variabile = names(contributi_baseline),
  Contributo = as.numeric(contributi_baseline)
)

df_contributi <- df_contributi[order(-abs(df_contributi$Contributo)), ]

ggplot(
  df_contributi,
  aes(
    x = reorder(Variabile, Contributo),
    y = Contributo,
    fill = Contributo > 0
  )
) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "darkgreen"), guide = "none") +
  labs(title = "Sales Contribution Decomposition", x = "Variable", y = "Contribution") +
  theme_minimal()

# ==============================================================================
# 5. ROI STRUCTURE
# ==============================================================================

# Actual investment values and the original average selling price are proprietary
# and are intentionally excluded from the public repository.

investimenti <- data.frame(
  Canale = c("TV", "Meta_YouTube", "Search"),
  Investimento = c(NA_real_, NA_real_, NA_real_)
)

vendite_generate <- data.frame(
  Canale = c("TV", "Meta_YouTube", "Search"),
  Vendite = c(
    contributi_baseline["TV"],
    contributi_baseline["Meta_YouTube"],
    contributi_baseline["Search"]
  )
)

prezzo_medio <- NA_real_

dati_roi <- merge(investimenti, vendite_generate, by = "Canale")
dati_roi$ROI <- (dati_roi$Vendite * prezzo_medio - dati_roi$Investimento) / dati_roi$Investimento * 100

tabella_roi <- dati_roi[, c("Canale", "Investimento", "Vendite", "ROI")]
print(tabella_roi)
