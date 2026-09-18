library(patchwork)
library(tidyverse)

setwd("~/Library/CloudStorage/OneDrive-JamesCookUniversity/Projects/THESIS_PHD_NLJ_2024/CH5_PREForaging")


# ---- 1. Four-way interaction grid: moon × light × tide × season ----
grid_4way <- expand.grid(
  nearestTide = seq(min(prePoints$nearestTide, na.rm = TRUE),
                    max(prePoints$nearestTide, na.rm = TRUE),
                    length.out = 100),
  moonFraction = c(0, 0.25, 0.5, 0.75, 1),
  lightPeriod = levels(prePoints$lightPeriod),
  season = levels(prePoints$season)
) %>%
  mutate(
    LOCBANDED = factor("HER", levels = levels(prePoints$LOCBANDED)),
    BandNumber = NA
  )

grid_4way$pred <- predict(m13, newdata = grid_4way, type = "response", re.form = NA)

p1 <- ggplot(grid_4way, aes(x = nearestTide, y = pred, color = factor(moonFraction))) +
  geom_line(size = 1) +
  facet_grid(season ~ lightPeriod) +
  labs(
    x = "Tide Height (m)",
    y = "P(onColony)",
    color = "Moon Fraction",
    title = "Effect of Moon, Light, Tide, and Season on On-Colony Probability"
  ) +
  theme_minimal(base_size = 13)

# ---- 2. Tide × Colony interaction grid ----
grid_colony <- expand.grid(
  nearestTide = seq(min(prePoints$nearestTide, na.rm = TRUE),
                    max(prePoints$nearestTide, na.rm = TRUE),
                    length.out = 100),
  LOCBANDED = levels(prePoints$LOCBANDED)
) %>%
  mutate(
    moonFraction = 0.5,
    lightPeriod = factor("Dark", levels = levels(prePoints$lightPeriod)),
    season = factor("Summer", levels = levels(prePoints$season)),
    BandNumber = NA
  )

grid_colony$pred <- predict(m13, newdata = grid_colony, type = "response", re.form = NA)

p2 <- ggplot(grid_colony, aes(x = nearestTide, y = pred, color = LOCBANDED)) +
  geom_line(size = 1.2) +
  labs(
    x = "Tide Height (m)",
    y = "P(onColony)",
    color = "Colony",
    title = "Colony-Specific Tide Response (Summer, Dark, MoonFraction = 0.5)"
  ) +
  theme_minimal(base_size = 13)

# Print both plots
p1+p2
