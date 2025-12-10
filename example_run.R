# example_run.R
# Demonstration script for running one simulation replicate.

library(ggplot2)
library(tidyr)

# Load the model
source("main_code_ITV.R")

# Run a short test simulation
results <- run_simulation(years = 10)

# Plot 1: Beetle population across years

beetle_plot <- ggplot(results, aes(x = year, y = total_beetles_all_gens)) +
  geom_line(color = "red", linewidth = 1) +
  labs(
    title = "Beetle Population Through Time",
    x = "Year",
    y = "Total Beetles (all generations)"
  ) +
  theme_minimal(base_size = 13)

print(beetle_plot)


# Plot 2: Plant population (total, glabrous, hairy)

# Prepare data for long format
plant_df <- data.frame(
  year = results$year,
  Total = results$total_plants,
  Glabrous = results$glabrous_plants,
  Hairy = results$hairy_plants
)

plant_long <- plant_df %>%
  pivot_longer(
    cols = -year,           
    names_to = "variable",
    values_to = "value"
  )

plan_pop_plot <- ggplot(plant_long, aes(x = year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Total" = "black",
      "Glabrous" = "lightgreen",
      "Hairy" = "darkgreen"
    )
  ) +
  labs(
    title = "Plant Population Through Time",
    x = "Year",
    y = "Number of Plants",
    color = "Plant Type"
  ) +
  theme_minimal(base_size = 13)

print(plan_pop_plot)


# Plot 3: Morph proportions (glabrous vs hairy)

prop_df <- data.frame(
  year = results$year,
  Glabrous = results$prop_glabrous,
  Hairy = results$prop_hairy
)

prop_long <- prop_df %>% 
  pivot_longer(
    cols = -year,
    names_to = "variable",
    values_to = "value"
  )

morph_prop_plot <- ggplot(prop_long, aes(x = year, y = value, color = variable)) +
  geom_line(linewidth = 1) +
  scale_color_manual(
    values = c(
      "Glabrous" = "lightgreen",
      "Hairy" = "darkgreen"
    )
  ) +
  labs(
    title = "Morph Proportions Through Time",
    x = "Year",
    y = "Proportion",
    color = "Morph"
  ) +
  theme_minimal(base_size = 13)

print(morph_prop_plot)


