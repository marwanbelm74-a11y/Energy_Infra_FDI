# ==============================================
# ENERGY INFRASTRUCTURE & FDI
# STEP 6 - REGIONAL ANALYSIS
# ==============================================

library(tidyverse)
library(fixest)

# 1. Importation
data <- read_csv(
  "energy_fdi_raw_data.csv",
  show_col_types = FALSE
)

# 2. Définir les régions
data <- data %>%
  mutate(
    region = case_when(
      iso2c %in% c(
        "NG", "KE", "GH", "SN", "CI",
        "MA", "EG", "TZ", "RW", "ZA"
      ) ~ "Afrique",
      
      iso2c %in% c(
        "IN", "BD", "PK", "ID", "VN",
        "PH", "TH", "KH", "LK", "NP"
      ) ~ "Asie",
      
      iso2c %in% c(
        "BR", "MX", "CO", "PE", "AR",
        "CL", "EC", "BO", "PY", "GT"
      ) ~ "Amerique_latine",
      
      TRUE ~ NA_character_
    ),
    
    log_pib = log(pib_habitant)
  )

# Vérifier le nombre de pays par région
data %>%
  distinct(iso3c, region) %>%
  count(region)

# 3. Définir l'Afrique comme référence
data <- data %>%
  mutate(
    region = factor(
      region,
      levels = c(
        "Afrique",
        "Asie",
        "Amerique_latine"
      )
    )
  )

# Modèle corrigé : coefficients pour les 3 régions

m_region2 <- feols(
  ide ~
    i(region, electricite) +
    i(region, pertes) +
    log_pib +
    croissance +
    commerce +
    inflation |
    iso3c + year,
  
  data = data,
  vcov = ~iso3c
)

# Afficher les résultats
summary(m_region2)

# 5. Résultats
summary(m_region)

# 6. Tableau
etable(m_region)

# Installer uniquement si nécessaire
install.packages("car")

library(car)

# Comparaison Afrique vs Asie : accès à l'électricité
linearHypothesis(
  m_region2,
  'region::Afrique:electricite = region::Asie:electricite',
  vcov. = vcov(m_region2),
  test = "F"
)

# Comparaison Afrique vs Asie : pertes électriques
linearHypothesis(
  m_region2,
  'region::Afrique:pertes = region::Asie:pertes',
  vcov. = vcov(m_region2),
  test = "F"
)

library(car)

# Test global : les coefficients d'accès
# à l'électricité sont-ils égaux entre régions ?

test_acces <- linearHypothesis(
  m_region2,
  c(
    "region::Afrique:electricite = region::Asie:electricite",
    "region::Afrique:electricite = region::Amerique_latine:electricite"
  ),
  vcov. = vcov(m_region2),
  test = "F"
)

# Test global : les coefficients des pertes
# électriques sont-ils égaux entre régions ?

test_pertes <- linearHypothesis(
  m_region2,
  c(
    "region::Afrique:pertes = region::Asie:pertes",
    "region::Afrique:pertes = region::Amerique_latine:pertes"
  ),
  vcov. = vcov(m_region2),
  test = "F"
)

# Afficher les résultats
test_acces
test_pertes

# Nombre de pays dans les données
n_distinct(data$iso3c)

# Nombre de pays utilisés dans le modèle
n_distinct(data$iso3c[-obs(m_region2)])

# Identifier le pays absent
setdiff(
  unique(data$iso3c),
  unique(data$iso3c[-obs(m_region2)])
)
# Pays présents dans les données
pays_initiaux <- unique(data$iso3c)

# Pays utilisés dans le modèle
pays_utilises <- unique(
  data$iso3c[obs(m_region2)]
)

# Vérifier
length(pays_initiaux)
length(pays_utilises)

# Pays absents
setdiff(pays_initiaux, pays_utilises)