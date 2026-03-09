# Load necessary libraries
library(tidyverse)
library(scales)

# 1. Setup Parameters
set.seed(42)
years <- 2024:2029
neighborhoods <- c("Sea Point", "Green Point", "Gardens", "CBD", "Woodstock", 
                   "Observatory", "Camps Bay", "Claremont", "Muizenberg", "Hout Bay")

# 2. Generate Base Data
synthetic_data <- expand.grid(Year = years, Neighborhood = neighborhoods) %>%
  group_by(Neighborhood) %>%
  mutate(
    # Initial STR density (percentage of housing stock)
    STR_Density = seq(runif(1, 2, 5), by = runif(1, 1.5, 3), length.out = n()),
    
    # Base rent (ZAR) with a 5% natural inflation + STR displacement factor
    # Formula: Base_Rent * (1 + Inflation)^Year * (1 + STR_Impact)
    Natural_Growth = 1.05^(Year - 2024),
    Displacement_Factor = 1 + (STR_Density / 15)^2, # Non-linear impact
    Projected_Monthly_Rent = (runif(1, 12000, 22000) * Natural_Growth * Displacement_Factor)
  ) %>%
  ungroup()

# 3. Define Regulatory Risk Threshold
# Intervention is likely if STR Density > 10% or Rent Growth > 40% from baseline
synthetic_data <- synthetic_data %>%
  group_by(Neighborhood) %>%
  mutate(
    Baseline_Rent = first(Projected_Monthly_Rent),
    Rent_Increase_Pct = (Projected_Monthly_Rent - Baseline_Rent) / Baseline_Rent,
    Reg_Risk_Score = (STR_Density * 0.6) + (Rent_Increase_Pct * 100 * 0.4)
  )

# 4. Visualization of the "Tipping Point"
ggplot(synthetic_data, aes(x = Year, y = Projected_Monthly_Rent, color = Neighborhood)) +
  geom_line(size = 1) +
  geom_point() +
  geom_hline(yintercept = 25000, linetype = "dashed", color = "red") +
  labs(
    title = "Cape Horizon Realty: Projected Rent Displacement (2024-2029)",
    subtitle = "Simulating the correlation between STR expansion and local housing affordability",
    y = "Average Monthly Rent (ZAR)",
    x = "Year"
  ) +
  theme_minimal()
