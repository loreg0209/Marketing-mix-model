# ==============================================================================
# 03_nonlinear_mmm.R
# Nonlinear Marketing Mix Model
#
# Thesis workflow:
# 1. C-shaped saturation + Adstock
# 2. initial simultaneous nonlinear specification
# 3. separate estimation of media transformation parameters
# 4. transformation of media variables
# 5. complete impact model
# 6. parsimonious final model
#
# Experimental S-shaped saturation functions are intentionally excluded because
# they were not part of the final thesis specification.
# ==============================================================================

library(minpack.lm)
library(lmtest)
library(ggplot2)
library(dplyr)

# ==============================================================================
# 1. C-SHAPED SATURATION + ADSTOCK
# ==============================================================================

Adstock_SatDecay1 <- function(df_shifted, alpha, decay) {
  vett_finale <- (df_shifted$NoShift)^alpha +
    decay * (df_shifted$Shift1)^alpha +
    decay^2 * (df_shifted$Shift2)^alpha +
    decay^3 * (df_shifted$Shift3)^alpha +
    decay^4 * (df_shifted$Shift4)^alpha +
    decay^5 * (df_shifted$Shift5)^alpha +
    decay^6 * (df_shifted$Shift6)^alpha

  return(vett_finale)
}

shift_df <- function(vettore, step) {
  dati_shifted <- data.frame(matrix(0, length(vettore), step + 1))
  names(dati_shifted) <- c("NoShift", paste0("Shift", 1:step))

  for (i in 0:step) {
    if (i == 0) {
      dati_shifted[, 1] <- vettore
    } else {
      dati_shifted[-(1:i), i + 1] <- vettore[1:(length(vettore) - i)]
    }
  }

  return(dati_shifted)
}

TV_target_shift <- shift_df(df$GRP_TV_target, 6)
TV_related_1_shift <- shift_df(df$GRP_TV_related_1, 6)
TV_related_2_shift <- shift_df(df$GRP_TV_related_2, 6)

imp_display_shift <- shift_df(df$imp_display, 6)
imp_meta_youtube_shift <- shift_df(df$imp_meta_youtube, 6)
imp_video_social_shift <- shift_df(df$imp_video_social, 6)
click_search_shift <- shift_df(df$search, 6)
competitors_shift <- shift_df(df$competition, 6)

# ==============================================================================
# 2. INITIAL SIMULTANEOUS ESTIMATION ATTEMPT
# ==============================================================================

# This first specification estimated baseline coefficients, media impacts,
# saturation and decay parameters simultaneously. It was used during model
# development but produced unstable / highly correlated estimates.

coeffs_complete_transform <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -Inf, -0.4),
  c_ref = c(1.5, 0, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, Inf),
  c_promo = c(14000, 10000, Inf),

  coeff_tv_target = c(0.5, 0, 20),
  alpha_tv_target = c(0.7, 0.5, 0.9),
  decay_tv_target = c(0.8, 0.3, 0.9),

  coeff_tv_related_1 = c(0, -4, 10),
  alpha_tv_related_1 = c(0.7, 0.6, 0.9),
  decay_tv_related_1 = c(0.7, 0.3, 0.9),

  coeff_tv_related_2 = c(0, -4, 4),
  alpha_tv_related_2 = c(0.6, 0.3, 0.9),
  decay_tv_related_2 = c(0.7, 0.6, 0.9),

  coeff_display = c(0.5, 0, 5),
  alpha_display = c(0.5, 0.2, 0.9),
  decay_display = c(0.5, 0.1, 0.9),

  coeff_meta_youtube = c(0.5, 0, 2),
  alpha_meta_youtube = c(0.5, 0.3, 0.9),
  decay_meta_youtube = c(0.5, 0.3, 0.9),

  coeff_video_social = c(0.5, 0, 2),
  alpha_video_social = c(0.5, 0.3, 0.7),
  decay_video_social = c(0.6, 0.2, 0.75),

  coeff_search = c(1, 0, 20),
  alpha_search = c(0.5, 0.2, 0.9),
  decay_search = c(0.5, 0.2, 0.9),

  coeff_competitors = c(0, -30, 2),
  alpha_competitors = c(0.3, 0.1, 0.9),
  decay_competitors = c(0.3, 0.1, 0.8)
))

formula_complete_transform <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (log(Base_price) ^ c_base_price) *
    (NEW_DATA_NMR ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    coeff_tv_target * Adstock_SatDecay1(TV_target_shift, alpha_tv_target, decay_tv_target) +
    coeff_tv_related_1 * Adstock_SatDecay1(TV_related_1_shift, alpha_tv_related_1, decay_tv_related_1) +
    coeff_tv_related_2 * Adstock_SatDecay1(TV_related_2_shift, alpha_tv_related_2, decay_tv_related_2) +
    coeff_display * Adstock_SatDecay1(imp_display_shift, alpha_display, decay_display) +
    coeff_meta_youtube * Adstock_SatDecay1(imp_meta_youtube_shift, alpha_meta_youtube, decay_meta_youtube) +
    coeff_video_social * Adstock_SatDecay1(imp_video_social_shift, alpha_video_social, decay_video_social) +
    coeff_search * Adstock_SatDecay1(click_search_shift, alpha_search, decay_search) +
    coeff_competitors * Adstock_SatDecay1(competitors_shift, alpha_competitors, decay_competitors)
)

start_values <- unlist(coeffs_complete_transform[1, ])
lower_bounds <- unlist(coeffs_complete_transform[2, ])
upper_bounds <- unlist(coeffs_complete_transform[3, ])

modello_nlsLM_complete_transform <- try(nlsLM(
  formula = formula_complete_transform,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_complete_transform) == "try-error") {
  print("The simultaneous nonlinear specification did not converge.")
} else {
  print(summary(modello_nlsLM_complete_transform))
  print(cov2cor(vcov(modello_nlsLM_complete_transform)))
}

# ==============================================================================
# 3. SEPARATE ESTIMATION OF MEDIA TRANSFORMATION PARAMETERS
# ==============================================================================

# To reduce instability and correlation among nonlinear estimators, the media
# transformation parameters were estimated separately before the final model.

risultati_df <- data.frame(
  mezzo = character(),
  parametro = character(),
  stima = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

aggiungi_risultati <- function(modello, nome_mezzo) {
  if (class(modello) != "try-error") {
    sommario <- summary(modello)
    coef_table <- sommario$coefficients

    for (i in 1:nrow(coef_table)) {
      risultati_df <<- rbind(risultati_df, data.frame(
        mezzo = nome_mezzo,
        parametro = rownames(coef_table)[i],
        stima = coef_table[i, "Estimate"],
        p_value = coef_table[i, "Pr(>|t|)"],
        stringsAsFactors = FALSE
      ))
    }
  }
}

# ------------------------------------------------------------------------------
# 3.1 Target TV
# ------------------------------------------------------------------------------

coeffs_tv_target <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.7),
  c_promo = c(14000, 10000, Inf),
  decay_tv_target = c(0.7, 0.5, 0.9)
))

formula_tv_target <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(TV_target_shift, 0.9, decay_tv_target)
)

start_values <- unlist(coeffs_tv_target[1, ])
lower_bounds <- unlist(coeffs_tv_target[2, ])
upper_bounds <- unlist(coeffs_tv_target[3, ])

modello_nlsLM_tv_target <- try(nlsLM(
  formula = formula_tv_target,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_tv_target) != "try-error") {
  print(summary(modello_nlsLM_tv_target))
}

aggiungi_risultati(modello_nlsLM_tv_target, "TV_target")

# ------------------------------------------------------------------------------
# 3.2 Related TV 1
# ------------------------------------------------------------------------------

coeffs_tv_related_1 <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.7),
  c_promo = c(14000, 10000, 18000),
  decay_tv_related_1 = c(0.7, 0.5, 0.9)
))

formula_tv_related_1 <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(TV_related_1_shift, 0.9, decay_tv_related_1)
)

start_values <- unlist(coeffs_tv_related_1[1, ])
lower_bounds <- unlist(coeffs_tv_related_1[2, ])
upper_bounds <- unlist(coeffs_tv_related_1[3, ])

modello_nlsLM_tv_related_1 <- try(nlsLM(
  formula = formula_tv_related_1,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_tv_related_1) != "try-error") {
  print(summary(modello_nlsLM_tv_related_1))
}

aggiungi_risultati(modello_nlsLM_tv_related_1, "TV_related_1")

# ------------------------------------------------------------------------------
# 3.3 Related TV 2
# ------------------------------------------------------------------------------

coeffs_tv_related_2 <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.2),
  c_promo = c(14000, 10000, 18000),
  decay_tv_related_2 = c(0.7, 0.6, 0.9)
))

formula_tv_related_2 <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(TV_related_2_shift, 0.9, decay_tv_related_2)
)

start_values <- unlist(coeffs_tv_related_2[1, ])
lower_bounds <- unlist(coeffs_tv_related_2[2, ])
upper_bounds <- unlist(coeffs_tv_related_2[3, ])

modello_nlsLM_tv_related_2 <- try(nlsLM(
  formula = formula_tv_related_2,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_tv_related_2) != "try-error") {
  print(summary(modello_nlsLM_tv_related_2))
}

aggiungi_risultati(modello_nlsLM_tv_related_2, "TV_related_2")

# ------------------------------------------------------------------------------
# 3.4 Video / Social
# ------------------------------------------------------------------------------

coeffs_video_social <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.2),
  c_promo = c(14000, 10000, 20000),
  decay_video_social = c(0.6, 0.2, 0.75)
))

formula_video_social <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(imp_video_social_shift, 0.4199, decay_video_social)
)

start_values <- unlist(coeffs_video_social[1, ])
lower_bounds <- unlist(coeffs_video_social[2, ])
upper_bounds <- unlist(coeffs_video_social[3, ])

modello_nlsLM_video_social <- try(nlsLM(
  formula = formula_video_social,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_video_social) != "try-error") {
  print(summary(modello_nlsLM_video_social))
}

aggiungi_risultati(modello_nlsLM_video_social, "Video_Social")

# ------------------------------------------------------------------------------
# 3.5 Meta / YouTube
# ------------------------------------------------------------------------------

coeffs_meta_youtube <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.2),
  c_promo = c(14000, 10000, 20000),
  alpha_meta_youtube = c(0.5, 0.3, 0.6),
  decay_meta_youtube = c(0.5, 0.3, 0.7)
))

formula_meta_youtube <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(imp_meta_youtube_shift, alpha_meta_youtube, decay_meta_youtube)
)

start_values <- unlist(coeffs_meta_youtube[1, ])
lower_bounds <- unlist(coeffs_meta_youtube[2, ])
upper_bounds <- unlist(coeffs_meta_youtube[3, ])

modello_nlsLM_meta_youtube <- try(nlsLM(
  formula = formula_meta_youtube,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_meta_youtube) != "try-error") {
  print(summary(modello_nlsLM_meta_youtube))
}

aggiungi_risultati(modello_nlsLM_meta_youtube, "Meta_YouTube")

# ------------------------------------------------------------------------------
# 3.6 Display
# ------------------------------------------------------------------------------

coeffs_display <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  c_promo = c(14000, 10000, Inf),
  alpha_display = c(0.5, 0.005, 0.9)
))

formula_display <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (38461.1211 + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(imp_display_shift, alpha_display, 0.1)
)

start_values <- unlist(coeffs_display[1, ])
lower_bounds <- unlist(coeffs_display[2, ])
upper_bounds <- unlist(coeffs_display[3, ])

modello_nlsLM_display <- try(nlsLM(
  formula = formula_display,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_display) != "try-error") {
  print(summary(modello_nlsLM_display))
}

aggiungi_risultati(modello_nlsLM_display, "Display")

# ------------------------------------------------------------------------------
# 3.7 Search
# ------------------------------------------------------------------------------

coeffs_search <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.2),
  c_promo = c(14000, 10000, 20000),
  decay_search = c(0.5, 0.3, 0.8)
))

formula_search <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(click_search_shift, 0.6039, decay_search)
)

start_values <- unlist(coeffs_search[1, ])
lower_bounds <- unlist(coeffs_search[2, ])
upper_bounds <- unlist(coeffs_search[3, ])

modello_nlsLM_search <- try(nlsLM(
  formula = formula_search,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_search) != "try-error") {
  print(summary(modello_nlsLM_search))
}

aggiungi_risultati(modello_nlsLM_search, "Search")

# ------------------------------------------------------------------------------
# 3.8 Competitor media pressure
# ------------------------------------------------------------------------------

coeffs_competitors <- as.data.frame(list(
  c_Stag = c(1, 0.5, 1.2),
  c_base_price = c(-0.7, -1.5, -0.4),
  c_ref = c(1.5, 1, 2.3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 1.2),
  c_promo = c(14000, 10000, 20000),
  alpha_competitors = c(0.5, 0.3, 0.9),
  decay_competitors = c(0.5, 0.3, 0.9)
))

formula_competitors <- as.formula(
  Vol_Sales ~
    (seasonal_index_prophet ^ c_Stag) *
    (Base_price ^ c_base_price) *
    (log(NEW_DATA_NMR) ^ c_ref) *
    (intercetta + c_promo * Promo_Intensity) +
    Adstock_SatDecay1(competitors_shift, alpha_competitors, decay_competitors)
)

start_values <- unlist(coeffs_competitors[1, ])
lower_bounds <- unlist(coeffs_competitors[2, ])
upper_bounds <- unlist(coeffs_competitors[3, ])

modello_nlsLM_competitors <- try(nlsLM(
  formula = formula_competitors,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_competitors) != "try-error") {
  print(summary(modello_nlsLM_competitors))
}

aggiungi_risultati(modello_nlsLM_competitors, "Competitors")
print(risultati_df)

# ==============================================================================
# 4. FINAL MEDIA TRANSFORMATIONS
# ==============================================================================

df$TV_target_transformed <- Adstock_SatDecay1(TV_target_shift, alpha = 0.9, decay = 0.9)
df$TV_related_1_transformed <- Adstock_SatDecay1(TV_related_1_shift, alpha = 0.9, decay = 0.9)
df$TV_related_2_transformed <- Adstock_SatDecay1(TV_related_2_shift, alpha = 0.9, decay = 0.9)

df$video_social_transformed <- Adstock_SatDecay1(imp_video_social_shift, alpha = 0.421, decay = 0.4603)
df$meta_youtube_transformed <- Adstock_SatDecay1(imp_meta_youtube_shift, alpha = 0.412, decay = 0.619)
df$search_transformed <- Adstock_SatDecay1(click_search_shift, alpha = 0.5990, decay = 0.5187)
df$competitors_transformed <- Adstock_SatDecay1(competitors_shift, alpha = 0.3, decay = 0.3)
df$display_transformed <- Adstock_SatDecay1(imp_display_shift, alpha = 0.9, decay = 0.84)

# ==============================================================================
# 5. COMPLETE IMPACT MODEL
# ==============================================================================

coeffs_complete <- as.data.frame(list(
  c_Stag = c(0.7, 0.5, 0.9),
  c_base_price = c(-0.8, -Inf, -0.4),
  c_ref = c(1, 0.2, 3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 2),
  c_promo = c(15000, 10000, Inf),

  c_target = c(1, 0, Inf),
  c_related_1 = c(1.5, 0.5, Inf),
  c_related_2 = c(0, -Inf, 3),
  c_display = c(1, 0, 5),
  c_meta_youtube = c(0.5, 0, 5),
  c_video_social = c(0.5, 0, 2),
  c_search = c(0.5, 0, Inf),
  c_competitors = c(-1, -Inf, 0)
))

formula_nlsLM_complete <- as.formula(
  Vol_Sales ~
    ((seasonal_index_prophet ^ c_Stag) *
       (log(Base_price) ^ c_base_price) *
       (I(NEW_DATA_NMR^0.5) ^ c_ref)) *
    (intercetta + c_promo * Promo_Intensity) +
    c_target * log(TV_target_transformed + 1) +
    c_related_1 * TV_related_1_transformed +
    c_related_2 * log(TV_related_2_transformed + 1) +
    c_display * display_transformed +
    c_meta_youtube * meta_youtube_transformed +
    c_video_social * video_social_transformed +
    c_search * search_transformed +
    c_competitors * competitors_transformed
)

start_values <- unlist(coeffs_complete[1, ])
lower_bounds <- unlist(coeffs_complete[2, ])
upper_bounds <- unlist(coeffs_complete[3, ])

modello_nlsLM_complete <- try(nlsLM(
  formula = formula_nlsLM_complete,
  data = df,
  start = start_values,
  lower = lower_bounds,
  upper = upper_bounds,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_complete) == "try-error") {
  print("The complete impact model did not converge.")
} else {
  print(summary(modello_nlsLM_complete))
}

vcov_complete <- vcov(modello_nlsLM_complete)
cor_complete <- cov2cor(vcov_complete)
print(cor_complete)

# ==============================================================================
# 6. PARSIMONIOUS FINAL MODEL
# ==============================================================================

coeffs_final <- as.data.frame(list(
  c_Stag = c(0.7, 0.5, 0.9),
  c_base_price = c(-0.8, -Inf, -0.4),
  c_ref = c(1, 0.2, 3),
  intercetta = c(mean(df$Vol_Sales) * 0.8, mean(df$Vol_Sales) * 0.5, mean(df$Vol_Sales) * 2),
  c_promo = c(15000, 10000, Inf),
  c_target = c(1, 0, Inf),
  c_meta_youtube = c(0.5, 0, 5),
  c_search = c(0.5, 0, Inf),
  c_competitors = c(-1, -Inf, 0)
))

formula_nlsLM_finale <- as.formula(
  Vol_Sales ~
    ((seasonal_index_prophet ^ c_Stag) *
       (log(Base_price) ^ c_base_price) *
       (I(NEW_DATA_NMR^0.5) ^ c_ref)) *
    (intercetta + c_promo * Promo_Intensity) +
    c_target * log(TV_target_transformed + 1) +
    c_meta_youtube * meta_youtube_transformed +
    c_search * search_transformed +
    c_competitors * competitors_transformed
)

start_values_final <- unlist(coeffs_final[1, ])
lower_bounds_final <- unlist(coeffs_final[2, ])
upper_bounds_final <- unlist(coeffs_final[3, ])

modello_nlsLM_finale <- try(nlsLM(
  formula = formula_nlsLM_finale,
  data = df,
  start = start_values_final,
  lower = lower_bounds_final,
  upper = upper_bounds_final,
  control = nls.lm.control(maxiter = 1000, ftol = 1e-8)
))

if (class(modello_nlsLM_finale) == "try-error") {
  stop("The final nonlinear model did not converge.")
}

print(summary(modello_nlsLM_finale))

# ==============================================================================
# 7. SATURATION CURVES
# ==============================================================================

saturation_curve_data <- function(intervallo_spesa, alpha, decay, passi = 6) {
  dati_spesa <- data.frame(spesa = intervallo_spesa)
  dati_shiftati <- shift_df(dati_spesa$spesa, passi)
  spesa_trasformata <- Adstock_SatDecay1(dati_shiftati, alpha, decay)

  return(data.frame(
    spesa = dati_spesa$spesa,
    trasformata = spesa_trasformata
  ))
}

dati_saturazione_tv <- saturation_curve_data(
  seq(0, max(df$GRP_TV_target, na.rm = TRUE), length.out = 100),
  alpha = 0.9,
  decay = 0.9
)

dati_saturazione_meta_youtube <- saturation_curve_data(
  seq(0, max(df$imp_meta_youtube, na.rm = TRUE), length.out = 150),
  alpha = 0.412,
  decay = 0.619
)

dati_saturazione_search <- saturation_curve_data(
  seq(0, max(df$search, na.rm = TRUE), length.out = 100),
  alpha = 0.5990,
  decay = 0.5187
)

dati_saturazione <- bind_rows(
  dati_saturazione_tv %>% mutate(Variabile = "TV", Metrica = spesa),
  dati_saturazione_meta_youtube %>% mutate(Variabile = "Meta / YouTube", Metrica = spesa),
  dati_saturazione_search %>% mutate(Variabile = "Search", Metrica = spesa)
)

ggplot(dati_saturazione, aes(x = Metrica, y = trasformata)) +
  geom_line() +
  facet_wrap(~ Variabile, nrow = 3, scales = "free") +
  labs(title = "C-Shaped Saturation Curves", x = "Media Activity", y = "Transformed Value") +
  theme_bw()
