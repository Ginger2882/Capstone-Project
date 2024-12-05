library(ggplot2)

scatter_plot <- ggplot(Factors_results, aes(x = Model, y = AIC)) +
  geom_point(color = "blue", size = 2) +  
  labs(title = "Scatter Plot of AIC for Models 1-255", 
       x = "Model", 
       y = "AIC") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

box_plot <- ggplot(Factors_results, aes(y = AIC)) +
  geom_boxplot(fill = "lightblue", color = "blue", alpha = 0.5) +
  labs(title = "Box Plot of AIC for Models 1-255", 
       x = "", 
       y = "AIC") +
  theme_minimal()

print(scatter_plot)
print(box_plot)
