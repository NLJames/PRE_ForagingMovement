library(ggplot2)
library(dplyr)
library(tidyr)

# Define warming scenarios with temp and RSLR range (m/year)
slr_scenarios <- tibble::tibble(
  temp = c("1.5°C", "2.0°C", "3.0°C", "4.0°C", "5.0°C"),
  rslr_mean = c(0.004, 0.006, 0.009, 0.011, 0.013),
  rslr_min = c(0.0024, 0.004, 0.006, 0.008, 0.010),
  rslr_max = c(0.0064, 0.007, 0.010, 0.012, 0.014)
) %>%
  mutate(
    scenario_label = paste0(
      temp, " (RSLR: ", rslr_min * 1000, "–", rslr_max * 1000, " mm/yr)"
    )
  )

# Expand across years
years <- 2024:2100
slr_data <- slr_scenarios %>%
  expand(scenario_label, Year = years) %>%
  left_join(slr_scenarios, by = "scenario_label") %>%
  mutate(
    slr_mean = rslr_mean * (Year - 2024),
    slr_min = rslr_min * (Year - 2024),
    slr_max = rslr_max * (Year - 2024)
  )

# Plot with enhanced legend
ggplot(slr_data, aes(x = Year, group = scenario_label)) +
  geom_ribbon(aes(ymin = slr_min, ymax = slr_max, fill = scenario_label), alpha = 0.2) +
  geom_line(aes(y = slr_mean, colour = scenario_label), linewidth = 1) +
  geom_hline(yintercept = 0.4, linetype = "dashed", colour = "red", linewidth = 1) +
  annotate("text", x = 2026, y = 0.41, label = "25% Foraging Loss [EXAMPLE ONLY]", colour = "red", hjust = 0, size = 4) +
  scale_y_continuous("Cumulative Sea-Level Rise (m)") +
  scale_x_continuous("Year", breaks = seq(2025, 2100, 5)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right") +
  labs(
    title = "Cumulative Sea-Level Rise by Warming Scenario and RSLR",
    fill = "Warming Level & RSLR",
    colour = "Warming Level & RSLR"
  )
