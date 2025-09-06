
min_train <- graph.GB_LC.fem %>%
  filter(rmse.train == min(rmse.train))

train_plot <- ggplot(graph.GB_LC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 3) +
  geom_point(data = min_train, color = "darkgreen", size = 3, shape = 2, stroke = 2) + 
  geom_text(data = min_train, aes(label = round(rmse.train, 4)), vjust = -2, hjust = 0.5, color = "darkgreen", size = 3) + 
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

min_test <- graph.GB_LC.fem %>%
  filter(rmse.test == min(rmse.test))

test_plot <- ggplot(graph.GB_LC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 3) +
  geom_point(data = min_test, color = "darkgreen", size = 3, shape = 2, stroke = 2) + 
  geom_text(data = min_test, aes(label = round(rmse.test, 4)), vjust = -2, hjust = 0.5, color = "darkgreen", size = 3) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

ttplot.SWE <- train_plot + test_plot +
  plot_annotation(
    title = "GB-LC SWEDEN",
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )
ggsave("SWE.TrainTest.plot.png", plot = ttplot.SWE, width = 8, height = 6, dpi = 600)

