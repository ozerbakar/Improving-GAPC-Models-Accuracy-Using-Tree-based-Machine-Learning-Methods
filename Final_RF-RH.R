
Deaths.test.RF.RH <- Deaths.test
Deaths.test.RF.RH$psi.RH <- 0

# Creating learner
rf <- makeLearner("regr.ranger", predict.type = "response")

# Creating training and testing tasks
trainTask.RF.RH <- makeRegrTask(data = Deaths.RH, target = "psi.RH")
testTask.RF.RH <- makeRegrTask(data = Deaths.test.RF.RH, target = "psi.RH")

# Creating parameters grid
rf_params <- expand.grid(
  mtry = c(1,2, 3, 4),
  num.trees = c(50, 100, 150, 200, 300),
  min.node.size = c(1:5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_RF.RH.train <- Deaths.RH
output_RF.RH.test <- Deaths.test.RF.RH

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
    task = trainTask.RF.RH,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.rf <- train(pars.rf, trainTask.RF.RH)
  
  # Prediction of the model
  preds.test.rf <- predict(model.rf, task = testTask.RF.RH)
  preds.train.rf <- predict(model.rf, task = trainTask.RF.RH)
  
  # Saving predictions
  output_RF.RH.test[[paste0("hp ",i)]] <- preds.test.rf$data$response
  output_RF.RH.train[[paste0("hp ",i)]] <- preds.train.rf$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.RH <- as.vector(as.matrix(for.m.RH.fem))
Rate.test.male.RH <- as.vector(as.matrix(for.m.RH.male))

test.psi.fem.RF.RH <- output_RF.RH.test %>% filter(Gender == "f")
test.psi.fem.RF.RH <- test.psi.fem.RF.RH[-1:-5]
test.psi.male.RF.RH <- output_RF.RH.test %>% filter(Gender == "m")
test.psi.male.RF.RH <- test.psi.male.RF.RH[-1:-5]

test.imp.rates.fem.RF.RH <- as.data.frame(as.matrix(test.psi.fem.RF.RH)*Rate.test.fem.RH)
test.imp.rates.fem.RF.RH$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.RF.RH <- as.data.frame(as.matrix(test.psi.male.RF.RH)*Rate.test.male.RH)
test.imp.rates.male.RF.RH$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.RF.RH.fem <- c()
test.rmse_results.RF.RH.male <- c()

for (i in colnames(test.imp.rates.fem.RF.RH)) {
  if (i != "obs.test") {
    test.rmse_val.RF.RH.fem <- RMSE(test.imp.rates.fem.RF.RH[[i]], test.imp.rates.fem.RF.RH$obs.test)
    test.rmse_results.RF.RH.fem[i] <- test.rmse_val.RF.RH.fem
  }
}

for (i in colnames(test.imp.rates.male.RF.RH)) {
  if (i != "obs.test") {
    test.rmse_val.RF.RH.male <- RMSE(test.imp.rates.male.RF.RH[[i]], test.imp.rates.male.RF.RH$obs.test)
    test.rmse_results.RF.RH.male[i] <- test.rmse_val.RF.RH.male
  }
}

test.rmse.RF.RH.fem <- data.frame(
  Hyper.p = names(test.rmse_results.RF.RH.fem),
  rmse.test = as.numeric(test.rmse_results.RF.RH.fem)
)
test.rmse.RF.RH.male <- data.frame(
  Hyper.p = names(test.rmse_results.RF.RH.male),
  rmse.test = as.numeric(test.rmse_results.RF.RH.male)
)

test.rmse.lower.RF.RH.fem <- test.rmse.RF.RH.fem %>% filter(rmse.test < rmse.for.RH.fem)
test.rmse.lower.RF.RH.male <- test.rmse.RF.RH.male %>% filter(rmse.test < rmse.for.RH.male)

Rate.train.fem.RH <- as.vector(as.matrix(m.RH.fem))
Rate.train.male.RH <- as.vector(as.matrix(m.RH.male))

train.psi.fem.RF.RH <- output_RF.RH.train %>% filter(Gender == "f")
train.psi.fem.RF.RH <- train.psi.fem.RF.RH[-1:-5]
train.psi.male.RF.RH <- output_RF.RH.train %>% filter(Gender == "m")
train.psi.male.RF.RH <- train.psi.male.RF.RH[-1:-5]

train.imp.rates.fem.RF.RH <- as.data.frame(as.matrix(train.psi.fem.RF.RH)*Rate.train.fem.RH)
train.imp.rates.fem.RF.RH$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.RF.RH <- as.data.frame(as.matrix(train.psi.male.RF.RH)*Rate.train.male.RH)
train.imp.rates.male.RF.RH$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.RF.RH.fem <- c()
train.rmse_results.RF.RH.male <- c()

for (i in colnames(train.imp.rates.fem.RF.RH)) {
  if (i != "obs.train") {
    train.rmse_val.RF.RH.fem <- RMSE(train.imp.rates.fem.RF.RH[[i]], train.imp.rates.fem.RF.RH$obs.train)
    train.rmse_results.RF.RH.fem[i] <- train.rmse_val.RF.RH.fem
  }
}
for (i in colnames(train.imp.rates.male.RF.RH)) {
  if (i != "obs.train") {
    train.rmse_val.RF.RH.male <- RMSE(train.imp.rates.male.RF.RH[[i]], train.imp.rates.male.RF.RH$obs.train)
    train.rmse_results.RF.RH.male[i] <- train.rmse_val.RF.RH.male
  }
}
train.rmse.RF.RH.fem <- data.frame(
  Hyper.p = names(train.rmse_results.RF.RH.fem),
  rmse.train = as.numeric(train.rmse_results.RF.RH.fem))
train.rmse.RF.RH.male <- data.frame(
  Hyper.p = names(train.rmse_results.RF.RH.male),
  rmse.train = as.numeric(train.rmse_results.RF.RH.male))

train.rmse.lower.RF.RH.fem <- train.rmse.RF.RH.fem %>% filter(rmse.train < rmse.fit.RH.fem)
tt.rmse.RF.RH.fem <- inner_join(test.rmse.lower.RF.RH.fem, train.rmse.lower.RF.RH.fem, by = "Hyper.p")
train.rmse.lower.RF.RH.male <- train.rmse.RF.RH.male %>% filter(rmse.train < rmse.fit.RH.male)
tt.rmse.RF.RH.male <- inner_join(test.rmse.lower.RF.RH.male, train.rmse.lower.RF.RH.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.RF.RH.fem$rmse.test <- round(tt.rmse.RF.RH.fem$rmse.test, digits = 8)
tt.rmse.RF.RH.fem$rmse.train <- round(tt.rmse.RF.RH.fem$rmse.train, digits = 8)
tt.rmse.RF.RH.male$rmse.test <- round(tt.rmse.RF.RH.male$rmse.test, digits = 8)
tt.rmse.RF.RH.male$rmse.train <- round(tt.rmse.RF.RH.male$rmse.train, digits = 8)

graph.RF_RH.fem <- tt.rmse.RF.RH.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.RF_RH.male <- tt.rmse.RF.RH.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.RF_RH.fem <- graph.RF_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.RF_RH.male <- graph.RF_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.RF_RH.fem <- graph.RF_RH.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.RF_RH.male <- graph.RF_RH.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.RF_RH.fem <- graph.RF_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.RF_RH.male <- graph.RF_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.RF_RH.fem$Hyper.p <- factor(graph.RF_RH.fem$Hyper.p,
                                   levels = graph.RF_RH.fem$Hyper.p[order(as.numeric(str_extract(graph.RF_RH.fem$Hyper.p, "[0-9]+")))])
graph.RF_RH.male$Hyper.p <- factor(graph.RF_RH.male$Hyper.p,
                                    levels = graph.RF_RH.male$Hyper.p[order(as.numeric(str_extract(graph.RF_RH.male$Hyper.p, "[0-9]+")))])

plot.RF.RH.fem.test <- ggplot(graph.RF_RH.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.RH.fem.train <- ggplot(graph.RF_RH.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.RH.fem.comb <- plot.RF.RH.fem.train + plot.RF.RH.fem.test + 
  plot_annotation(title = "RF-RH Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.RH.fem.comb)

plot.RF.RH.male.test <- ggplot(graph.RF_RH.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.RH.male.train <- ggplot(graph.RF_RH.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.RH.male.comb <- plot.RF.RH.male.train + plot.RF.RH.male.test + 
  plot_annotation(title = "RF-RH Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.RH.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.RF_RH.fem) {
  keep <- rep(TRUE, nrow(graph.RF_RH.fem))
  for (i in 1:nrow(graph.RF_RH.fem)) {
    for (j in 1:nrow(graph.RF_RH.fem)) {
      if (i != j) {
        if (graph.RF_RH.fem$rmse.train[j] <= graph.RF_RH.fem$rmse.train[i] &&
            graph.RF_RH.fem$rmse.test[j]  <= graph.RF_RH.fem$rmse.test[i] &&
            (graph.RF_RH.fem$rmse.train[j] < graph.RF_RH.fem$rmse.train[i] ||
             graph.RF_RH.fem$rmse.test[j]  < graph.RF_RH.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_RH.fem$pareto <- pareto_front(graph.RF_RH.fem)

# Plot
Plot.EFF.RF.RH.fem <- ggplot(graph.RF_RH.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_RH.fem[graph.RF_RH.fem$pareto, ][order(graph.RF_RH.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_RH.fem[graph.RF_RH.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-RH Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.RF_RH.male) {
  keep <- rep(TRUE, nrow(graph.RF_RH.male))
  for (i in 1:nrow(graph.RF_RH.male)) {
    for (j in 1:nrow(graph.RF_RH.male)) {
      if (i != j) {
        if (graph.RF_RH.male$rmse.train[j] <= graph.RF_RH.male$rmse.train[i] &&
            graph.RF_RH.male$rmse.test[j]  <= graph.RF_RH.male$rmse.test[i] &&
            (graph.RF_RH.male$rmse.train[j] < graph.RF_RH.male$rmse.train[i] ||
             graph.RF_RH.male$rmse.test[j]  < graph.RF_RH.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_RH.male$pareto <- pareto_front(graph.RF_RH.male)

# Plot
Plot.EFF.RF.RH.male <- ggplot(graph.RF_RH.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_RH.male[graph.RF_RH.male$pareto, ][order(graph.RF_RH.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_RH.male[graph.RF_RH.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-RH Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.RF.RH.comb <- Plot.EFF.RF.RH.fem + Plot.EFF.RF.RH.male 
print(Plot.EFF.RF.RH.comb)
