# =====================================================
# ENERGY INFRASTRUCTURE & FOREIGN DIRECT INVESTMENT
# STEP 3 - DESCRIPTIVE ANALYSIS
# =====================================================

library(tidyverse)

# 1. Importer notre base de données
data <- read_csv("energy_fdi_raw_data.csv")

# 2. Statistiques descriptives
statistiques <- data %>%
  summarise(
    nombre_pays = n_distinct(iso3c),
    nombre_annees = n_distinct(year),
    
    ide_moyen = mean(ide, na.rm = TRUE),
    ide_median = median(ide, na.rm = TRUE),
    
    acces_electricite_moyen =
      mean(electricite, na.rm = TRUE),
    
    pertes_moyennes =
      mean(pertes, na.rm = TRUE),
    
    croissance_moyenne =
      mean(croissance, na.rm = TRUE)
  )

print(statistiques)


# 3. Evolution moyenne des IDE
evolution_ide <- data %>%
  group_by(year) %>%
  summarise(
    ide_moyen = mean(ide, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(evolution_ide, aes(x = year, y = ide_moyen)) +
  geom_line(color = "#2563EB", linewidth = 1.2) +
  geom_point(color = "#2563EB") +
  labs(
    title = "Evolution des IDE dans les pays emergents",
    subtitle = "Moyenne de 30 pays, 2000-2024",
    x = "Annee",
    y = "IDE entrants nets (% du PIB)",
    caption = "Source : World Bank - WDI"
  ) +
  theme_minimal()


# 4. Evolution de l'acces a l'electricite
evolution_electricite <- data %>%
  group_by(year) %>%
  summarise(
    acces_moyen = mean(electricite, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(
  evolution_electricite,
  aes(x = year, y = acces_moyen)
) +
  geom_line(color = "#16A34A", linewidth = 1.2) +
  geom_point(color = "#16A34A") +
  labs(
    title = "Developpement de l'acces a l'electricite",
    subtitle = "Moyenne de 30 pays, 2000-2024",
    x = "Annee",
    y = "Population ayant acces a l'electricite (%)",
    caption = "Source : World Bank - WDI"
  ) +
  theme_minimal()


# 5. Relation entre electricite et IDE
ggplot(
  data,
  aes(x = electricite, y = ide)
) +
  geom_point(
    alpha = 0.4,
    color = "#2563EB"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#DC2626"
  ) +
  labs(
    title = "Electricite et investissements etrangers",
    subtitle = "30 pays emergents, 2000-2024",
    x = "Acces a l'electricite (%)",
    y = "IDE entrants nets (% du PIB)",
    caption = "Source : World Bank - WDI"
  ) +
  theme_minimal()
# ==========================================
# RELATION ENTRE PERTES ELECTRIQUES ET IDE
# ==========================================

ggplot(
  data,
  aes(x = pertes, y = ide)
) +
  geom_point(
    alpha = 0.4,
    color = "#2563EB"
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#DC2626"
  ) +
  labs(
    title = "Pertes electriques et investissements etrangers",
    subtitle = "30 pays emergents, 2000-2024",
    x = "Pertes electriques (% de la production)",
    y = "IDE entrants nets (% du PIB)",
    caption = "Source : World Bank - WDI"
  ) +
  theme_minimal()

# ==========================================
# 6. EXPORT DES GRAPHIQUES POUR GITHUB
# ==========================================

# Creer le dossier figures
dir.create("figures", showWarnings = FALSE)

# Graphique 1 : Evolution des IDE
graphique_ide <- ggplot(evolution_ide, aes(x = year, y = ide_moyen)) +
  geom_line(color = "#2563EB", linewidth = 1.2) +
  geom_point(color = "#2563EB") +
  labs(
    title = "FDI inflows in emerging economies",
    subtitle = "Average across 30 countries, 2000–2024",
    x = "Year",
    y = "Net FDI inflows (% of GDP)",
    caption = "Source: World Bank - WDI"
  ) +
  theme_minimal()

ggsave("figures/01_fdi_trend.png", graphique_ide, width = 10, height = 6, dpi = 300)


# Graphique 2 : Acces a l'electricite
graphique_electricite <- ggplot(evolution_electricite, aes(x = year, y = acces_moyen)) +
  geom_line(color = "#16A34A", linewidth = 1.2) +
  geom_point(color = "#16A34A") +
  labs(
    title = "Access to electricity in emerging economies",
    subtitle = "Average across 30 countries, 2000–2024",
    x = "Year",
    y = "Population with access to electricity (%)",
    caption = "Source: World Bank - WDI"
  ) +
  theme_minimal()

ggsave("figures/02_electricity_access.png", graphique_electricite, width = 10, height = 6, dpi = 300)


# Graphique 3 : Electricite et IDE
graphique_relation <- ggplot(data, aes(x = electricite, y = ide)) +
  geom_point(alpha = 0.4, color = "#2563EB") +
  geom_smooth(method = "lm", se = TRUE, color = "#DC2626") +
  labs(
    title = "Electricity access and foreign direct investment",
    subtitle = "30 emerging economies, 2000–2024",
    x = "Access to electricity (%)",
    y = "Net FDI inflows (% of GDP)",
    caption = "Source: World Bank - WDI"
  ) +
  theme_minimal()

ggsave("figures/03_electricity_fdi.png", graphique_relation, width = 10, height = 6, dpi = 300)


# Graphique 4 : Pertes electriques et IDE
graphique_pertes <- ggplot(data, aes(x = pertes, y = ide)) +
  geom_point(alpha = 0.4, color = "#2563EB") +
  geom_smooth(method = "lm", se = TRUE, color = "#DC2626") +
  labs(
    title = "Electricity losses and foreign direct investment",
    subtitle = "30 emerging economies, 2000–2024",
    x = "Electricity losses (% of output)",
    y = "Net FDI inflows (% of GDP)",
    caption = "Source: World Bank - WDI"
  ) +
  theme_minimal()

ggsave("figures/04_electricity_losses_fdi.png", graphique_pertes, width = 10, height = 6, dpi = 300)

print("All four figures have been saved successfully!")