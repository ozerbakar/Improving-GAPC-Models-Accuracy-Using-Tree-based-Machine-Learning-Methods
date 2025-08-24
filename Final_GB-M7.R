
Deaths.test.GB.M7 <- Deaths.test
Deaths.test.GB.M7$psi.M7 <- 0

# Creating learner
gb <- makeLearner("regr.gbm", predict.type = "response")

# Creating training and testing tasks
trainTask.GB.M7 <- makeRegrTask(data = Deaths.M7, target = "psi.M7")
testTask.GB.M7 <- makeRegrTask(data = Deaths.test.GB.M7, target = "psi.M7")

# Creating parameters grid
gb_params <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  n.trees = c(50, 100, 150),
  interaction.depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_GB.M7.train <- Deaths.M7
output_GB.M7.test <- Deaths.test.GB.M7

# Calibration of psi over all hyper-parameter combinations
for (i in 1:nrow(gb_params)) {
  hp.gb <- gb_params[i, ]
  
  # Selecting hyper-parameter set
  pars.gb <- setHyperPars(
    learner = gb,
    par.vals = list(
      shrinkage = hp.gb$shrinkage,
      n.trees = hp.gb$n.trees,
      interaction.depth = hp.gb$interaction.depth
    )
  )
  
  # Cross-validation
  resample_result <- resample(
    learner = pars.gb,
    task = trainTask.GB.M7,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.gb <- train(pars.gb, trainTask.GB.M7)
  
  # Prediction of the model
  preds.test.gb <- predict(model.gb, task = testTask.GB.M7)
  preds.train.gb <- predict(model.gb, task = trainTask.GB.M7)
  
  # Saving predictions
  output_GB.M7.test[[paste0("hp ",i)]] <- preds.test.gb$data$response
  output_GB.M7.train[[paste0("hp ",i)]] <- preds.train.gb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.M7 <- as.vector(as.matrix(for.m.M7.fem))
Rate.test.male.M7 <- as.vector(as.matrix(for.m.M7.male))

test.psi.fem.GB.M7 <- output_GB.M7.test %>% filter(Gender == "f")
test.psi.fem.GB.M7 <- test.psi.fem.GB.M7[-1:-5]
test.psi.male.GB.M7 <- output_GB.M7.test %>% filter(Gender == "m")
test.psi.male.GB.M7 <- test.psi.male.GB.M7[-1:-5]

test.imp.rates.fem.GB.M7 <- as.data.frame(as.matrix(test.psi.fem.GB.M7)*Rate.test.fem.M7)
test.imp.rates.fem.GB.M7$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.GB.M7 <- as.data.frame(as.matrix(test.psi.male.GB.M7)*Rate.test.male.M7)
test.imp.rates.male.GB.M7$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.GB.M7.fem <- c()
test.rmse_results.GB.M7.male <- c()

for (i in colnames(test.imp.rates.fem.GB.M7)) {
  if (i != "obs.test") {
    test.rmse_val.GB.M7.fem <- RMSE(test.imp.rates.fem.GB.M7[[i]], test.imp.rates.fem.GB.M7$obs.test)
    test.rmse_results.GB.M7.fem[i] <- test.rmse_val.GB.M7.fem
  }
}

for (i in colnames(test.imp.rates.male.GB.M7)) {
  if (i != "obs.test") {
    test.rmse_val.GB.M7.male <- RMSE(test.imp.rates.male.GB.M7[[i]], test.imp.rates.male.GB.M7$obs.test)
    test.rmse_results.GB.M7.male[i] <- test.rmse_val.GB.M7.male
  }
}

test.rmse.GB.M7.fem <- data.frame(
  Hyper.p = names(test.rmse_results.GB.M7.fem),
  rmse.test = as.numeric(test.rmse_results.GB.M7.fem)
)
test.rmse.GB.M7.male <- data.frame(
  Hyper.p = names(test.rmse_results.GB.M7.male),
  rmse.test = as.numeric(test.rmse_results.GB.M7.male)
)

test.rmse.lower.GB.M7.fem <- test.rmse.GB.M7.fem %>% filter(rmse.test < rmse.for.M7.fem)
test.rmse.lower.GB.M7.male <- test.rmse.GB.M7.male %>% filter(rmse.test < rmse.for.M7.male)

Rate.train.fem.M7 <- as.vector(as.matrix(m.M7.fem))
Rate.train.male.M7 <- as.vector(as.matrix(m.M7.male))

train.psi.fem.GB.M7 <- output_GB.M7.train %>% filter(Gender == "f")
train.psi.fem.GB.M7 <- train.psi.fem.GB.M7[-1:-5]
train.psi.male.GB.M7 <- output_GB.M7.train %>% filter(Gender == "m")
train.psi.male.GB.M7 <- train.psi.male.GB.M7[-1:-5]

train.imp.rates.fem.GB.M7 <- as.data.frame(as.matrix(train.psi.fem.GB.M7)*Rate.train.fem.M7)
train.imp.rates.fem.GB.M7$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.GB.M7 <- as.data.frame(as.matrix(train.psi.male.GB.M7)*Rate.train.male.M7)
train.imp.rates.male.GB.M7$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.GB.M7.fem <- c()
train.rmse_results.GB.M7.male <- c()

for (i in colnames(train.imp.rates.fem.GB.M7)) {
  if (i != "obs.train") {
    train.rmse_val.GB.M7.fem <- RMSE(train.imp.rates.fem.GB.M7[[i]], train.imp.rates.fem.GB.M7$obs.train)
    train.rmse_results.GB.M7.fem[i] <- train.rmse_val.GB.M7.fem
  }
}
for (i in colnames(train.imp.rates.male.GB.M7)) {
  if (i != "obs.train") {
    train.rmse_val.GB.M7.male <- RMSE(train.imp.rates.male.GB.M7[[i]], train.imp.rates.male.GB.M7$obs.train)
    train.rmse_results.GB.M7.male[i] <- train.rmse_val.GB.M7.male
  }
}
train.rmse.GB.M7.fem <- data.frame(
  Hyper.p = names(train.rmse_results.GB.M7.fem),
  rmse.train = as.numeric(train.rmse_results.GB.M7.fem))
train.rmse.GB.M7.male <- data.frame(
  Hyper.p = names(train.rmse_results.GB.M7.male),
  rmse.train = as.numeric(train.rmse_results.GB.M7.male))

train.rmse.lower.GB.M7.fem <- train.rmse.GB.M7.fem %>% filter(rmse.train < rmse.fit.M7.fem)
tt.rmse.GB.M7.fem <- inner_join(test.rmse.lower.GB.M7.fem, train.rmse.lower.GB.M7.fem, by = "Hyper.p")
train.rmse.lower.GB.M7.male <- train.rmse.GB.M7.male %>% filter(rmse.train < rmse.fit.M7.male)
tt.rmse.GB.M7.male <- inner_join(test.rmse.lower.GB.M7.male, train.rmse.lower.GB.M7.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.GB.M7.fem$rmse.test <- round(tt.rmse.GB.M7.fem$rmse.test, digits = 8)
tt.rmse.GB.M7.fem$rmse.train <- round(tt.rmse.GB.M7.fem$rmse.train, digits = 8)
tt.rmse.GB.M7.male$rmse.test <- round(tt.rmse.GB.M7.male$rmse.test, digits = 8)
tt.rmse.GB.M7.male$rmse.train <- round(tt.rmse.GB.M7.male$rmse.train, digits = 8)

graph.GB_M7.fem <- tt.rmse.GB.M7.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.GB_M7.male <- tt.rmse.GB.M7.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.GB_M7.fem <- graph.GB_M7.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.GB_M7.male <- graph.GB_M7.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.GB_M7.fem <- graph.GB_M7.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.GB_M7.male <- graph.GB_M7.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.GB_M7.fem <- graph.GB_M7.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.GB_M7.male <- graph.GB_M7.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.GB_M7.fem$Hyper.p <- factor(graph.GB_M7.fem$Hyper.p,
                                   levels = graph.GB_M7.fem$Hyper.p[order(as.numeric(str_extract(graph.GB_M7.fem$Hyper.p, "[0-9]+")))])
graph.GB_M7.male$Hyper.p <- factor(graph.GB_M7.male$Hyper.p,
                                    levels = graph.GB_M7.male$Hyper.p[order(as.numeric(str_extract(graph.GB_M7.male$Hyper.p, "[0-9]+")))])

plot.GB.M7.fem.test <- ggplot(graph.GB_M7.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.M7.fem.train <- ggplot(graph.GB_M7.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.M7.fem.comb <- plot.GB.M7.fem.train + plot.GB.M7.fem.test + 
  plot_annotation(title = "GB-M7 Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.M7.fem.comb)

plot.GB.M7.male.test <- ggplot(graph.GB_M7.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.M7.male.train <- ggplot(graph.GB_M7.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.M7.male.comb <- plot.GB.M7.male.train + plot.GB.M7.male.test + 
  plot_annotation(title = "GB-M7 Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.M7.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.GB_M7.fem) {
  keep <- rep(TRUE, nrow(graph.GB_M7.fem))
  for (i in 1:nrow(graph.GB_M7.fem)) {
    for (j in 1:nrow(graph.GB_M7.fem)) {
      if (i != j) {
        if (graph.GB_M7.fem$rmse.train[j] <= graph.GB_M7.fem$rmse.train[i] &&
            graph.GB_M7.fem$rmse.test[j]  <= graph.GB_M7.fem$rmse.test[i] &&
            (graph.GB_M7.fem$rmse.train[j] < graph.GB_M7.fem$rmse.train[i] ||
             graph.GB_M7.fem$rmse.test[j]  < graph.GB_M7.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_M7.fem$pareto <- pareto_front(graph.GB_M7.fem)

# Plot
Plot.EFF.GB.M7.fem <- ggplot(graph.GB_M7.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_M7.fem[graph.GB_M7.fem$pareto, ][order(graph.GB_M7.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_M7.fem[graph.GB_M7.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-M7 Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.GB_M7.male) {
  keep <- rep(TRUE, nrow(graph.GB_M7.male))
  for (i in 1:nrow(graph.GB_M7.male)) {
    for (j in 1:nrow(graph.GB_M7.male)) {
      if (i != j) {
        if (graph.GB_M7.male$rmse.train[j] <= graph.GB_M7.male$rmse.train[i] &&
            graph.GB_M7.male$rmse.test[j]  <= graph.GB_M7.male$rmse.test[i] &&
            (graph.GB_M7.male$rmse.train[j] < graph.GB_M7.male$rmse.train[i] ||
             graph.GB_M7.male$rmse.test[j]  < graph.GB_M7.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_M7.male$pareto <- pareto_front(graph.GB_M7.male)

# Plot
Plot.EFF.GB.M7.male <- ggplot(graph.GB_M7.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_M7.male[graph.GB_M7.male$pareto, ][order(graph.GB_M7.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_M7.male[graph.GB_M7.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-M7 Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.GB.M7.comb <- Plot.EFF.GB.M7.fem + Plot.EFF.GB.M7.male 
print(Plot.EFF.GB.M7.comb)
