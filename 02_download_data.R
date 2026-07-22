# ==================================================
# ENERGY INFRASTRUCTURE & FDI
# STEP 2 - DATA COLLECTION
# ==================================================

library(WDI)
library(tidyverse)

# 1. Sélection des 30 pays
pays <- c(
  "NG", "KE", "GH", "SN", "CI",
  "MA", "EG", "TZ", "RW", "ZA",
  "IN", "BD", "PK", "ID", "VN",
  "PH", "TH", "KH", "LK", "NP",
  "BR", "MX", "CO", "PE", "AR",
  "CL", "EC", "BO", "PY", "GT"
)

# 2. Indicateurs économiques
indicateurs <- c(
  ide = "BX.KLT.DINV.WD.GD.ZS",
  electricite = "EG.ELC.ACCS.ZS",
  pertes = "EG.ELC.LOSS.ZS",
  pib_habitant = "NY.GDP.PCAP.KD",
  croissance = "NY.GDP.MKTP.KD.ZG",
  commerce = "NE.TRD.GNFS.ZS",
  inflation = "FP.CPI.TOTL.ZG"
)

# 3. Téléchargement depuis la Banque mondiale
data <- WDI(
  country = pays,
  indicator = indicateurs,
  start = 2000,
  end = 2024
)

# 4. Trier les observations
data <- data %>%
  arrange(iso2c, year)

# 5. Afficher les premières lignes
head(data, 10)

# 6. Vérifier les dimensions
dim(data)

# 7. Vérifier les valeurs manquantes
colSums(is.na(data))

# 8. Sauvegarder les données
write_csv(data, "energy_fdi_raw_data.csv")