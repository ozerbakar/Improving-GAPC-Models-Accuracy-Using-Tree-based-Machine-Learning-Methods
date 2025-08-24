
Deaths.test.RF.LC <- Deaths.test
Deaths.test.RF.LC$psi.LC <- 0

# Creating learner
rf <- makeLearner("regr.ranger", predict.type = "response")

# Creating training and testing tasks
trainTask.RF.LC <- makeRegrTask(data = Deaths.LC, target = "psi.LC")
testTask.RF.LC <- makeRegrTask(data = Deaths.test.RF.LC, target = "psi.LC")

# Creating parameters grid
rf_params <- expand.grid(
  mtry = c(1,2, 3, 4),
  num.trees = c(50, 100, 150, 200, 300),
  min.node.size = c(1:5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_RF.LC.train <- Deaths.LC
output_RF.LC.test <- Deaths.test.RF.LC

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
    task = trainTask.RF.LC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.rf <- train(pars.rf, trainTask.RF.LC)
  
  # Prediction of the model
  preds.test.rf <- predict(model.rf, task = testTask.RF.LC)
  preds.train.rf <- predict(model.rf, task = trainTask.RF.LC)
  
  # Saving predictions
  output_RF.LC.test[[paste0("hp ",i)]] <- preds.test.rf$data$response
  output_RF.LC.train[[paste0("hp ",i)]] <- preds.train.rf$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.LC <- as.vector(as.matrix(for.m.LC.fem))
Rate.test.male.LC <- as.vector(as.matrix(for.m.LC.male))

test.psi.fem.RF.LC <- output_RF.LC.test %>% filter(Gender == "f")
test.psi.fem.RF.LC <- test.psi.fem.RF.LC[-1:-5]
test.psi.male.RF.LC <- output_RF.LC.test %>% filter(Gender == "m")
test.psi.male.RF.LC <- test.psi.male.RF.LC[-1:-5]

test.imp.rates.fem.RF.LC <- as.data.frame(as.matrix(test.psi.fem.RF.LC)*Rate.test.fem.LC)
test.imp.rates.fem.RF.LC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.RF.LC <- as.data.frame(as.matrix(test.psi.male.RF.LC)*Rate.test.male.LC)
test.imp.rates.male.RF.LC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.RF.LC.fem <- c()
test.rmse_results.RF.LC.male <- c()

for (i in colnames(test.imp.rates.fem.RF.LC)) {
  if (i != "obs.test") {
    test.rmse_val.RF.LC.fem <- RMSE(test.imp.rates.fem.RF.LC[[i]], test.imp.rates.fem.RF.LC$obs.test)
    test.rmse_results.RF.LC.fem[i] <- test.rmse_val.RF.LC.fem
  }
}

for (i in colnames(test.imp.rates.male.RF.LC)) {
  if (i != "obs.test") {
    test.rmse_val.RF.LC.male <- RMSE(test.imp.rates.male.RF.LC[[i]], test.imp.rates.male.RF.LC$obs.test)
    test.rmse_results.RF.LC.male[i] <- test.rmse_val.RF.LC.male
  }
}

test.rmse.RF.LC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.RF.LC.fem),
  rmse.test = as.numeric(test.rmse_results.RF.LC.fem)
)
test.rmse.RF.LC.male <- data.frame(
  Hyper.p = names(test.rmse_results.RF.LC.male),
  rmse.test = as.numeric(test.rmse_results.RF.LC.male)
)

test.rmse.lower.RF.LC.fem <- test.rmse.RF.LC.fem %>% filter(rmse.test < rmse.for.LC.fem)
test.rmse.lower.RF.LC.male <- test.rmse.RF.LC.male %>% filter(rmse.test < rmse.for.LC.male)

Rate.train.fem.LC <- as.vector(as.matrix(m.LC.fem))
Rate.train.male.LC <- as.vector(as.matrix(m.LC.male))

train.psi.fem.RF.LC <- output_RF.LC.train %>% filter(Gender == "f")
train.psi.fem.RF.LC <- train.psi.fem.RF.LC[-1:-5]
train.psi.male.RF.LC <- output_RF.LC.train %>% filter(Gender == "m")
train.psi.male.RF.LC <- train.psi.male.RF.LC[-1:-5]

train.imp.rates.fem.RF.LC <- as.data.frame(as.matrix(train.psi.fem.RF.LC)*Rate.train.fem.LC)
train.imp.rates.fem.RF.LC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.RF.LC <- as.data.frame(as.matrix(train.psi.male.RF.LC)*Rate.train.male.LC)
train.imp.rates.male.RF.LC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.RF.LC.fem <- c()
train.rmse_results.RF.LC.male <- c()

for (i in colnames(train.imp.rates.fem.RF.LC)) {
  if (i != "obs.train") {
    train.rmse_val.RF.LC.fem <- RMSE(train.imp.rates.fem.RF.LC[[i]], train.imp.rates.fem.RF.LC$obs.train)
    train.rmse_results.RF.LC.fem[i] <- train.rmse_val.RF.LC.fem
  }
}
for (i in colnames(train.imp.rates.male.RF.LC)) {
  if (i != "obs.train") {
    train.rmse_val.RF.LC.male <- RMSE(train.imp.rates.male.RF.LC[[i]], train.imp.rates.male.RF.LC$obs.train)
    train.rmse_results.RF.LC.male[i] <- train.rmse_val.RF.LC.male
  }
}
train.rmse.RF.LC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.RF.LC.fem),
  rmse.train = as.numeric(train.rmse_results.RF.LC.fem))
train.rmse.RF.LC.male <- data.frame(
  Hyper.p = names(train.rmse_results.RF.LC.male),
  rmse.train = as.numeric(train.rmse_results.RF.LC.male))

train.rmse.lower.RF.LC.fem <- train.rmse.RF.LC.fem %>% filter(rmse.train < rmse.fit.LC.fem)
tt.rmse.RF.LC.fem <- inner_join(test.rmse.lower.RF.LC.fem, train.rmse.lower.RF.LC.fem, by = "Hyper.p")
train.rmse.lower.RF.LC.male <- train.rmse.RF.LC.male %>% filter(rmse.train < rmse.fit.LC.male)
tt.rmse.RF.LC.male <- inner_join(test.rmse.lower.RF.LC.male, train.rmse.lower.RF.LC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.RF.LC.fem$rmse.test <- round(tt.rmse.RF.LC.fem$rmse.test, digits = 8)
tt.rmse.RF.LC.fem$rmse.train <- round(tt.rmse.RF.LC.fem$rmse.train, digits = 8)
tt.rmse.RF.LC.male$rmse.test <- round(tt.rmse.RF.LC.male$rmse.test, digits = 8)
tt.rmse.RF.LC.male$rmse.train <- round(tt.rmse.RF.LC.male$rmse.train, digits = 8)

graph.RF_LC.fem <- tt.rmse.RF.LC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.RF_LC.male <- tt.rmse.RF.LC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.RF_LC.fem <- graph.RF_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.RF_LC.male <- graph.RF_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.RF_LC.fem <- graph.RF_LC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.RF_LC.male <- graph.RF_LC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.RF_LC.fem <- graph.RF_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.RF_LC.male <- graph.RF_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.RF_LC.fem$Hyper.p <- factor(graph.RF_LC.fem$Hyper.p,
                                  levels = graph.RF_LC.fem$Hyper.p[order(as.numeric(str_extract(graph.RF_LC.fem$Hyper.p, "[0-9]+")))])
graph.RF_LC.male$Hyper.p <- factor(graph.RF_LC.male$Hyper.p,
                                   levels = graph.RF_LC.male$Hyper.p[order(as.numeric(str_extract(graph.RF_LC.male$Hyper.p, "[0-9]+")))])

plot.RF.LC.fem.test <- ggplot(graph.RF_LC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.LC.fem.train <- ggplot(graph.RF_LC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.LC.fem.comb <- plot.RF.LC.fem.train + plot.RF.LC.fem.test + 
  plot_annotation(title = "RF-LC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.LC.fem.comb)

plot.RF.LC.male.test <- ggplot(graph.RF_LC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.LC.male.train <- ggplot(graph.RF_LC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.LC.male.comb <- plot.RF.LC.male.train + plot.RF.LC.male.test + 
  plot_annotation(title = "RF-LC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.LC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.RF_LC.fem) {
  keep <- rep(TRUE, nrow(graph.RF_LC.fem))
  for (i in 1:nrow(graph.RF_LC.fem)) {
    for (j in 1:nrow(graph.RF_LC.fem)) {
      if (i != j) {
        if (graph.RF_LC.fem$rmse.train[j] <= graph.RF_LC.fem$rmse.train[i] &&
            graph.RF_LC.fem$rmse.test[j]  <= graph.RF_LC.fem$rmse.test[i] &&
            (graph.RF_LC.fem$rmse.train[j] < graph.RF_LC.fem$rmse.train[i] ||
             graph.RF_LC.fem$rmse.test[j]  < graph.RF_LC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_LC.fem$pareto <- pareto_front(graph.RF_LC.fem)

# Plot
Plot.EFF.RF.LC.fem <- ggplot(graph.RF_LC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_LC.fem[graph.RF_LC.fem$pareto, ][order(graph.RF_LC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_LC.fem[graph.RF_LC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-LC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.RF_LC.male) {
  keep <- rep(TRUE, nrow(graph.RF_LC.male))
  for (i in 1:nrow(graph.RF_LC.male)) {
    for (j in 1:nrow(graph.RF_LC.male)) {
      if (i != j) {
        if (graph.RF_LC.male$rmse.train[j] <= graph.RF_LC.male$rmse.train[i] &&
            graph.RF_LC.male$rmse.test[j]  <= graph.RF_LC.male$rmse.test[i] &&
            (graph.RF_LC.male$rmse.train[j] < graph.RF_LC.male$rmse.train[i] ||
             graph.RF_LC.male$rmse.test[j]  < graph.RF_LC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_LC.male$pareto <- pareto_front(graph.RF_LC.male)

# Plot

Plot.EFF.RF.LC.male <- ggplot(graph.RF_LC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_LC.male[graph.RF_LC.male$pareto, ][order(graph.RF_LC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_LC.male[graph.RF_LC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-LC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.RF.LC.comb <- Plot.EFF.RF.LC.fem + Plot.EFF.RF.LC.male 
print(Plot.EFF.RF.LC.comb)
