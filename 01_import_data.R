# ==============================================
# ENERGY INFRASTRUCTURE & FDI RESEARCH
# STEP 1 - INSTALL PACKAGES
# ==============================================

packages <- c(
  "WDI",
  "tidyverse",
  "fixest",
  "modelsummary"
)

# Installer uniquement les packages manquants
to_install <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(to_install) > 0) {
  install.packages(to_install)
}

# Charger les packages
library(WDI)
library(tidyverse)
library(fixest)
library(modelsummary)