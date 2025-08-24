
Deaths.test.RF.APC <- Deaths.test
Deaths.test.RF.APC$psi.APC <- 0

# Creating learner
rf <- makeLearner("regr.ranger", predict.type = "response")

# Creating training and testing tasks
trainTask.RF.APC <- makeRegrTask(data = Deaths.APC, target = "psi.APC")
testTask.RF.APC <- makeRegrTask(data = Deaths.test.RF.APC, target = "psi.APC")

# Creating parameters grid
rf_params <- expand.grid(
  mtry = c(1,2, 3, 4),
  num.trees = c(50, 100, 150, 200, 300),
  min.node.size = c(1:5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_RF.APC.train <- Deaths.APC
output_RF.APC.test <- Deaths.test.RF.APC

# Calibration of psi over all hyper-parameter combinations
for (i in 1:nrow(rf_params)) {
  hp.rf <- rf_params[i, ]
  
  # Selecting hyper-parameter set
  pars.rf <- setHyperPars(
    learner = rf,
    par.vals = list(
      mtry = hp.rf$mtry,
      num.trees = hp.rf$num.trees,
      min.node.size = hp.rf$min.node.size
    )
  )
  
  # Cross-validation
  resample_result <- resample(
    learner = pars.rf,
    task = trainTask.RF.APC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.rf <- train(pars.rf, trainTask.RF.APC)
  
  # Prediction of the model
  preds.test.rf <- predict(model.rf, task = testTask.RF.APC)
  preds.train.rf <- predict(model.rf, task = trainTask.RF.APC)
  
  # Saving predictions
  output_RF.APC.test[[paste0("hp ",i)]] <- preds.test.rf$data$response
  output_RF.APC.train[[paste0("hp ",i)]] <- preds.train.rf$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.APC <- as.vector(as.matrix(for.m.APC.fem))
Rate.test.male.APC <- as.vector(as.matrix(for.m.APC.male))

test.psi.fem.RF.APC <- output_RF.APC.test %>% filter(Gender == "f")
test.psi.fem.RF.APC <- test.psi.fem.RF.APC[-1:-5]
test.psi.male.RF.APC <- output_RF.APC.test %>% filter(Gender == "m")
test.psi.male.RF.APC <- test.psi.male.RF.APC[-1:-5]

test.imp.rates.fem.RF.APC <- as.data.frame(as.matrix(test.psi.fem.RF.APC)*Rate.test.fem.APC)
test.imp.rates.fem.RF.APC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.RF.APC <- as.data.frame(as.matrix(test.psi.male.RF.APC)*Rate.test.male.APC)
test.imp.rates.male.RF.APC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.RF.APC.fem <- c()
test.rmse_results.RF.APC.male <- c()

for (i in colnames(test.imp.rates.fem.RF.APC)) {
  if (i != "obs.test") {
    test.rmse_val.RF.APC.fem <- RMSE(test.imp.rates.fem.RF.APC[[i]], test.imp.rates.fem.RF.APC$obs.test)
    test.rmse_results.RF.APC.fem[i] <- test.rmse_val.RF.APC.fem
  }
}

for (i in colnames(test.imp.rates.male.RF.APC)) {
  if (i != "obs.test") {
    test.rmse_val.RF.APC.male <- RMSE(test.imp.rates.male.RF.APC[[i]], test.imp.rates.male.RF.APC$obs.test)
    test.rmse_results.RF.APC.male[i] <- test.rmse_val.RF.APC.male
  }
}

test.rmse.RF.APC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.RF.APC.fem),
  rmse.test = as.numeric(test.rmse_results.RF.APC.fem)
)
test.rmse.RF.APC.male <- data.frame(
  Hyper.p = names(test.rmse_results.RF.APC.male),
  rmse.test = as.numeric(test.rmse_results.RF.APC.male)
)

test.rmse.lower.RF.APC.fem <- test.rmse.RF.APC.fem %>% filter(rmse.test < rmse.for.APC.fem)
test.rmse.lower.RF.APC.male <- test.rmse.RF.APC.male %>% filter(rmse.test < rmse.for.APC.male)

Rate.train.fem.APC <- as.vector(as.matrix(m.APC.fem))
Rate.train.male.APC <- as.vector(as.matrix(m.APC.male))

train.psi.fem.RF.APC <- output_RF.APC.train %>% filter(Gender == "f")
train.psi.fem.RF.APC <- train.psi.fem.RF.APC[-1:-5]
train.psi.male.RF.APC <- output_RF.APC.train %>% filter(Gender == "m")
train.psi.male.RF.APC <- train.psi.male.RF.APC[-1:-5]

train.imp.rates.fem.RF.APC <- as.data.frame(as.matrix(train.psi.fem.RF.APC)*Rate.train.fem.APC)
train.imp.rates.fem.RF.APC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.RF.APC <- as.data.frame(as.matrix(train.psi.male.RF.APC)*Rate.train.male.APC)
train.imp.rates.male.RF.APC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.RF.APC.fem <- c()
train.rmse_results.RF.APC.male <- c()

for (i in colnames(train.imp.rates.fem.RF.APC)) {
  if (i != "obs.train") {
    train.rmse_val.RF.APC.fem <- RMSE(train.imp.rates.fem.RF.APC[[i]], train.imp.rates.fem.RF.APC$obs.train)
    train.rmse_results.RF.APC.fem[i] <- train.rmse_val.RF.APC.fem
  }
}
for (i in colnames(train.imp.rates.male.RF.APC)) {
  if (i != "obs.train") {
    train.rmse_val.RF.APC.male <- RMSE(train.imp.rates.male.RF.APC[[i]], train.imp.rates.male.RF.APC$obs.train)
    train.rmse_results.RF.APC.male[i] <- train.rmse_val.RF.APC.male
  }
}
train.rmse.RF.APC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.RF.APC.fem),
  rmse.train = as.numeric(train.rmse_results.RF.APC.fem))
train.rmse.RF.APC.male <- data.frame(
  Hyper.p = names(train.rmse_results.RF.APC.male),
  rmse.train = as.numeric(train.rmse_results.RF.APC.male))

train.rmse.lower.RF.APC.fem <- train.rmse.RF.APC.fem %>% filter(rmse.train < rmse.fit.APC.fem)
tt.rmse.RF.APC.fem <- inner_join(test.rmse.lower.RF.APC.fem, train.rmse.lower.RF.APC.fem, by = "Hyper.p")
train.rmse.lower.RF.APC.male <- train.rmse.RF.APC.male %>% filter(rmse.train < rmse.fit.APC.male)
tt.rmse.RF.APC.male <- inner_join(test.rmse.lower.RF.APC.male, train.rmse.lower.RF.APC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.RF.APC.fem$rmse.test <- round(tt.rmse.RF.APC.fem$rmse.test, digits = 8)
tt.rmse.RF.APC.fem$rmse.train <- round(tt.rmse.RF.APC.fem$rmse.train, digits = 8)
tt.rmse.RF.APC.male$rmse.test <- round(tt.rmse.RF.APC.male$rmse.test, digits = 8)
tt.rmse.RF.APC.male$rmse.train <- round(tt.rmse.RF.APC.male$rmse.train, digits = 8)

graph.RF_APC.fem <- tt.rmse.RF.APC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.RF_APC.male <- tt.rmse.RF.APC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.RF_APC.fem <- graph.RF_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.RF_APC.male <- graph.RF_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.RF_APC.fem <- graph.RF_APC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.RF_APC.male <- graph.RF_APC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.RF_APC.fem <- graph.RF_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.RF_APC.male <- graph.RF_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.RF_APC.fem$Hyper.p <- factor(graph.RF_APC.fem$Hyper.p,
                                   levels = graph.RF_APC.fem$Hyper.p[order(as.numeric(str_extract(graph.RF_APC.fem$Hyper.p, "[0-9]+")))])
graph.RF_APC.male$Hyper.p <- factor(graph.RF_APC.male$Hyper.p,
                                    levels = graph.RF_APC.male$Hyper.p[order(as.numeric(str_extract(graph.RF_APC.male$Hyper.p, "[0-9]+")))])

plot.RF.APC.fem.test <- ggplot(graph.RF_APC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.APC.fem.train <- ggplot(graph.RF_APC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.APC.fem.comb <- plot.RF.APC.fem.train + plot.RF.APC.fem.test + 
  plot_annotation(title = "RF-APC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.APC.fem.comb)

plot.RF.APC.male.test <- ggplot(graph.RF_APC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.APC.male.train <- ggplot(graph.RF_APC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.APC.male.comb <- plot.RF.APC.male.train + plot.RF.APC.male.test + 
  plot_annotation(title = "RF-APC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.APC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.RF_APC.fem) {
  keep <- rep(TRUE, nrow(graph.RF_APC.fem))
  for (i in 1:nrow(graph.RF_APC.fem)) {
    for (j in 1:nrow(graph.RF_APC.fem)) {
      if (i != j) {
        if (graph.RF_APC.fem$rmse.train[j] <= graph.RF_APC.fem$rmse.train[i] &&
            graph.RF_APC.fem$rmse.test[j]  <= graph.RF_APC.fem$rmse.test[i] &&
            (graph.RF_APC.fem$rmse.train[j] < graph.RF_APC.fem$rmse.train[i] ||
             graph.RF_APC.fem$rmse.test[j]  < graph.RF_APC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_APC.fem$pareto <- pareto_front(graph.RF_APC.fem)

# Plot
Plot.EFF.RF.APC.fem <- ggplot(graph.RF_APC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_APC.fem[graph.RF_APC.fem$pareto, ][order(graph.RF_APC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_APC.fem[graph.RF_APC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-APC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.RF_APC.male) {
  keep <- rep(TRUE, nrow(graph.RF_APC.male))
  for (i in 1:nrow(graph.RF_APC.male)) {
    for (j in 1:nrow(graph.RF_APC.male)) {
      if (i != j) {
        if (graph.RF_APC.male$rmse.train[j] <= graph.RF_APC.male$rmse.train[i] &&
            graph.RF_APC.male$rmse.test[j]  <= graph.RF_APC.male$rmse.test[i] &&
            (graph.RF_APC.male$rmse.train[j] < graph.RF_APC.male$rmse.train[i] ||
             graph.RF_APC.male$rmse.test[j]  < graph.RF_APC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_APC.male$pareto <- pareto_front(graph.RF_APC.male)

# Plot
Plot.EFF.RF.APC.male <- ggplot(graph.RF_APC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_APC.male[graph.RF_APC.male$pareto, ][order(graph.RF_APC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_APC.male[graph.RF_APC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-APC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.RF.APC.comb <- Plot.EFF.RF.APC.fem + Plot.EFF.RF.APC.male 
print(Plot.EFF.RF.APC.comb)
