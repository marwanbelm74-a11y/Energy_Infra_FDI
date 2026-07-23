# ==========================================
# ENERGY INFRASTRUCTURE & FDI
# ECONOMETRIC ANALYSIS
# ==========================================

library(tidyverse)
library(fixest)

# Importer les données
data <- read_csv("energy_fdi_raw_data.csv")

# Préparation
data <- data %>%
  mutate(
    log_pib = log(pib_habitant)
  )

# Modèle 1 : accès à l'électricité
m1 <- feols(
  ide ~ electricite,
  data = data,
  vcov = ~iso3c
)

# Modèle 2 : pertes électriques
m2 <- feols(
  ide ~ pertes,
  data = data,
  vcov = ~iso3c
)

# Modèle 3 : modèle complet avec effets fixes
m3 <- feols(
  ide ~ electricite + pertes +
    log_pib + croissance +
    commerce + inflation |
    iso3c + year,
  data = data,
  vcov = ~iso3c
)

# Afficher les résultats
etable(m1, m2, m3)