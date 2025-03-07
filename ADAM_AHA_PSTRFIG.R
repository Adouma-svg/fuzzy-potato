
#BAR CHART

library(ggplot2)

# Creating the data
data <- data.frame(
  Phenotype = c("MHNW", "MHOW", "MHOB", "MUNW", "MUOW", "MUOB"),
  Prevalence = c(8.8, 8.7, 4.5, 12.4, 28.6, 37.1)
)

# Setting the correct order of the phenotypes to match the original figure
data$Phenotype <- factor(data$Phenotype, levels = c("MUOB", "MUOW", "MUNW", "MHOB", "MHOW", "MHNW"))

ggplot(data, aes(x = Prevalence, y = Phenotype)) +
  geom_bar(stat = "identity", fill = "gray50", color = "red", size = 1.5) +
  geom_text(aes(label = paste0(Prevalence, "%")), 
            color = "red", 
            size = 6, 
            hjust = -0.2,
            fontface = "bold") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(color = "red", size = 17, face = "bold"),
    axis.title.x = element_text(color = "red", size = 17, face = "bold"),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14, face = "bold"),
    panel.grid.major.x = element_line(color = "red", linetype = "dashed"),
    panel.grid.minor.x = element_line(color = "red", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(color = "black", size = 20, face = "bold", hjust = 0.5)
  ) +
  labs(
    x = "Prevalence (%)",
    y = "Phenotypes",
    title = "Prevalence of Metabolic Health Body Mass Phenotypes"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.1)))





#FOREST PLOT FOR CHRONIC STRESS: THOMAS PLEASE HELP!

library(ggplot2)

# Creating the dataset here
data <- data.frame(
  Phenotype = c("MHOW", "MHOB", "MUNW", "MUOW", "MUOB"),
  Odds_Ratio = c(0.93, 1.19, 1.02, 1.11, 1.19),
  CI_Lower = c(0.79, 1.02, 0.9, 0.99, 1.07),
  CI_Upper = c(1.08, 1.38, 1.16, 1.26, 1.33),
  Adjusted_Odds_Ratio = c(0.91, 1.13, 0.93, 1.00, 1.04),
  Adj_CI_Lower = c(0.78, 0.96, 0.82, 0.88, 0.93),
  Adj_CI_Upper = c(1.05, 1.32, 1.06, 1.13, 1.16)
)


plot_data <- data.frame(
  Phenotype = rep(data$Phenotype, each = 2),
  Type = rep(c("Unadjusted", "Adjusted"), times = nrow(data)),
  Odds_Ratio = c(data$Odds_Ratio, data$Adjusted_Odds_Ratio),
  CI_Lower = c(data$CI_Lower, data$Adj_CI_Lower),
  CI_Upper = c(data$CI_Upper, data$Adj_CI_Upper)
)


plot_data$y_position <- rep(seq(from = 1, by = 1, length.out = nrow(data)), each = 2)
plot_data$y_position <- ifelse(plot_data$Type == "Unadjusted", 
                               plot_data$y_position + 0.2, 
                               plot_data$y_position - 0.2)

forest_plot <- ggplot(plot_data, aes(x = Odds_Ratio, y = y_position, shape = Type, color = Type)) +
  geom_point(size = 4) +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2) +
  scale_shape_manual(values = c(16, 17)) + # Circle for Unadjusted, Triangle for Adjusted
  scale_color_manual(values = c("blue", "red")) +
  geom_vline(aes(xintercept = 1, linetype = "No Effect (OR = 1.0)"), 
             color = "grey", size = 1, show.legend = TRUE) +
  scale_linetype_manual(name = "Reference", values = "dotted") +
  theme_minimal() +
  labs(title = "Chronic Stress Associations",
       x = "Odds Ratio",
       y = "Phenotype",
       shape = "Type",
       color = "Type") +
  scale_y_continuous(breaks = seq(1, nrow(data), 1), 
                     labels = rev(data$Phenotype)) +
  theme(
    panel.grid.major.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), # Ensure title is centered and bold
    axis.title.x = element_text(size = 16, face = "bold"), # Bold and large x-axis title
    axis.title.y = element_text(size = 16, face = "bold"), # Bold and large y-axis title
    axis.text.y = element_text(size = 14, face = "bold"), # Bold and large y-axis labels
    axis.text.x = element_text(size = 12), # Larger x-axis text
    legend.title = element_text(size = 14, face = "bold"), # Bold legend title
    legend.text = element_text(size = 12) # Larger legend text
  ) +
  geom_text(aes(label = round(Odds_Ratio, 2)), vjust = -1, size = 5, fontface = "bold")


print(forest_plot)


ggsave("Chronic_Stress_Associations.png", plot = forest_plot, 
       width = 10, height = 6, dpi = 300, bg = "white")


