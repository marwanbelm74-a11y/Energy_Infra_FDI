# ==============================================
# ENERGY INFRASTRUCTURE & FDI
# STEP 5 - LAGGED EFFECTS
# ==============================================

library(tidyverse)
library(fixest)

# 1. Importation
data <- read_csv(
  "energy_fdi_raw_data.csv",
  show_col_types = FALSE
)

# 2. Préparation
data <- data %>%
  arrange(iso3c, year) %>%
  mutate(
    log_pib = if_else(
      pib_habitant > 0,
      log(pib_habitant),
      NA_real_
    )
  ) %>%
  group_by(iso3c) %>%
  mutate(
    electricite_lag1 = lag(electricite, 1),
    electricite_lag2 = lag(electricite, 2),
    electricite_lag3 = lag(electricite, 3),
    
    pertes_lag1 = lag(pertes, 1),
    pertes_lag2 = lag(pertes, 2),
    pertes_lag3 = lag(pertes, 3)
  ) %>%
  ungroup()

# 3. Échantillon commun aux trois modèles
# Cela garantit une comparaison sur les mêmes observations.

data_lags <- data %>%
  filter(
    if_all(
      c(
        ide,
        electricite_lag1,
        electricite_lag2,
        electricite_lag3,
        pertes_lag1,
        pertes_lag2,
        pertes_lag3,
        log_pib,
        croissance,
        commerce,
        inflation
      ),
      ~ !is.na(.)
    )
  )

# 4. Modèle avec retard d'un an
m_lag1 <- feols(
  ide ~ electricite_lag1 + pertes_lag1 +
    log_pib + croissance +
    commerce + inflation |
    iso3c + year,
  data = data_lags,
  vcov = ~iso3c
)

# 5. Modèle avec retard de deux ans
m_lag2 <- feols(
  ide ~ electricite_lag2 + pertes_lag2 +
    log_pib + croissance +
    commerce + inflation |
    iso3c + year,
  data = data_lags,
  vcov = ~iso3c
)

# 6. Modèle avec retard de trois ans
m_lag3 <- feols(
  ide ~ electricite_lag3 + pertes_lag3 +
    log_pib + croissance +
    commerce + inflation |
    iso3c + year,
  data = data_lags,
  vcov = ~iso3c
)

# 7. Comparaison
etable(
  m_lag1,
  m_lag2,
  m_lag3,
  digits = 4
)

# 8. Taille de l'échantillon
nrow(data_lags)

# Test conjoint des variables électriques

wald(
  m_lag1,
  "electricite_lag1|pertes_lag1"
)

wald(
  m_lag2,
  "electricite_lag2|pertes_lag2"
)

wald(
  m_lag3,
  "electricite_lag3|pertes_lag3"
)