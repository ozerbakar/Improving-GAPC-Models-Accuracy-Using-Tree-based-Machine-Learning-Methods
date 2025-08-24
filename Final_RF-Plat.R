
Deaths.test.RF.Plat <- Deaths.test
Deaths.test.RF.Plat$psi.Plat <- 0

# Creating learner
rf <- makeLearner("regr.ranger", predict.type = "response")

# Creating training and testing tasks
trainTask.RF.Plat <- makeRegrTask(data = Deaths.Plat, target = "psi.Plat")
testTask.RF.Plat <- makeRegrTask(data = Deaths.test.RF.Plat, target = "psi.Plat")

# Creating parameters grid
rf_params <- expand.grid(
  mtry = c(1,2, 3, 4),
  num.trees = c(50, 100, 150, 200, 300),
  min.node.size = c(1:5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_RF.Plat.train <- Deaths.Plat
output_RF.Plat.test <- Deaths.test.RF.Plat

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
    task = trainTask.RF.Plat,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.rf <- train(pars.rf, trainTask.RF.Plat)
  
  # Prediction of the model
  preds.test.rf <- predict(model.rf, task = testTask.RF.Plat)
  preds.train.rf <- predict(model.rf, task = trainTask.RF.Plat)
  
  # Saving predictions
  output_RF.Plat.test[[paste0("hp ",i)]] <- preds.test.rf$data$response
  output_RF.Plat.train[[paste0("hp ",i)]] <- preds.train.rf$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.Plat <- as.vector(as.matrix(for.m.Plat.fem))
Rate.test.male.Plat <- as.vector(as.matrix(for.m.Plat.male))

test.psi.fem.RF.Plat <- output_RF.Plat.test %>% filter(Gender == "f")
test.psi.fem.RF.Plat <- test.psi.fem.RF.Plat[-1:-5]
test.psi.male.RF.Plat <- output_RF.Plat.test %>% filter(Gender == "m")
test.psi.male.RF.Plat <- test.psi.male.RF.Plat[-1:-5]

test.imp.rates.fem.RF.Plat <- as.data.frame(as.matrix(test.psi.fem.RF.Plat)*Rate.test.fem.Plat)
test.imp.rates.fem.RF.Plat$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.RF.Plat <- as.data.frame(as.matrix(test.psi.male.RF.Plat)*Rate.test.male.Plat)
test.imp.rates.male.RF.Plat$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.RF.Plat.fem <- c()
test.rmse_results.RF.Plat.male <- c()

for (i in colnames(test.imp.rates.fem.RF.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.RF.Plat.fem <- RMSE(test.imp.rates.fem.RF.Plat[[i]], test.imp.rates.fem.RF.Plat$obs.test)
    test.rmse_results.RF.Plat.fem[i] <- test.rmse_val.RF.Plat.fem
  }
}

for (i in colnames(test.imp.rates.male.RF.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.RF.Plat.male <- RMSE(test.imp.rates.male.RF.Plat[[i]], test.imp.rates.male.RF.Plat$obs.test)
    test.rmse_results.RF.Plat.male[i] <- test.rmse_val.RF.Plat.male
  }
}

test.rmse.RF.Plat.fem <- data.frame(
  Hyper.p = names(test.rmse_results.RF.Plat.fem),
  rmse.test = as.numeric(test.rmse_results.RF.Plat.fem)
)
test.rmse.RF.Plat.male <- data.frame(
  Hyper.p = names(test.rmse_results.RF.Plat.male),
  rmse.test = as.numeric(test.rmse_results.RF.Plat.male)
)

test.rmse.lower.RF.Plat.fem <- test.rmse.RF.Plat.fem %>% filter(rmse.test < rmse.for.Plat.fem)
test.rmse.lower.RF.Plat.male <- test.rmse.RF.Plat.male %>% filter(rmse.test < rmse.for.Plat.male)

Rate.train.fem.Plat <- as.vector(as.matrix(m.Plat.fem))
Rate.train.male.Plat <- as.vector(as.matrix(m.Plat.male))

train.psi.fem.RF.Plat <- output_RF.Plat.train %>% filter(Gender == "f")
train.psi.fem.RF.Plat <- train.psi.fem.RF.Plat[-1:-5]
train.psi.male.RF.Plat <- output_RF.Plat.train %>% filter(Gender == "m")
train.psi.male.RF.Plat <- train.psi.male.RF.Plat[-1:-5]

train.imp.rates.fem.RF.Plat <- as.data.frame(as.matrix(train.psi.fem.RF.Plat)*Rate.train.fem.Plat)
train.imp.rates.fem.RF.Plat$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.RF.Plat <- as.data.frame(as.matrix(train.psi.male.RF.Plat)*Rate.train.male.Plat)
train.imp.rates.male.RF.Plat$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.RF.Plat.fem <- c()
train.rmse_results.RF.Plat.male <- c()

for (i in colnames(train.imp.rates.fem.RF.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.RF.Plat.fem <- RMSE(train.imp.rates.fem.RF.Plat[[i]], train.imp.rates.fem.RF.Plat$obs.train)
    train.rmse_results.RF.Plat.fem[i] <- train.rmse_val.RF.Plat.fem
  }
}
for (i in colnames(train.imp.rates.male.RF.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.RF.Plat.male <- RMSE(train.imp.rates.male.RF.Plat[[i]], train.imp.rates.male.RF.Plat$obs.train)
    train.rmse_results.RF.Plat.male[i] <- train.rmse_val.RF.Plat.male
  }
}
train.rmse.RF.Plat.fem <- data.frame(
  Hyper.p = names(train.rmse_results.RF.Plat.fem),
  rmse.train = as.numeric(train.rmse_results.RF.Plat.fem))
train.rmse.RF.Plat.male <- data.frame(
  Hyper.p = names(train.rmse_results.RF.Plat.male),
  rmse.train = as.numeric(train.rmse_results.RF.Plat.male))

train.rmse.lower.RF.Plat.fem <- train.rmse.RF.Plat.fem %>% filter(rmse.train < rmse.fit.Plat.fem)
tt.rmse.RF.Plat.fem <- inner_join(test.rmse.lower.RF.Plat.fem, train.rmse.lower.RF.Plat.fem, by = "Hyper.p")
train.rmse.lower.RF.Plat.male <- train.rmse.RF.Plat.male %>% filter(rmse.train < rmse.fit.Plat.male)
tt.rmse.RF.Plat.male <- inner_join(test.rmse.lower.RF.Plat.male, train.rmse.lower.RF.Plat.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.RF.Plat.fem$rmse.test <- round(tt.rmse.RF.Plat.fem$rmse.test, digits = 8)
tt.rmse.RF.Plat.fem$rmse.train <- round(tt.rmse.RF.Plat.fem$rmse.train, digits = 8)
tt.rmse.RF.Plat.male$rmse.test <- round(tt.rmse.RF.Plat.male$rmse.test, digits = 8)
tt.rmse.RF.Plat.male$rmse.train <- round(tt.rmse.RF.Plat.male$rmse.train, digits = 8)

graph.RF_Plat.fem <- tt.rmse.RF.Plat.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.RF_Plat.male <- tt.rmse.RF.Plat.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.RF_Plat.fem <- graph.RF_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.RF_Plat.male <- graph.RF_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.RF_Plat.fem <- graph.RF_Plat.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.RF_Plat.male <- graph.RF_Plat.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.RF_Plat.fem <- graph.RF_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.RF_Plat.male <- graph.RF_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.RF_Plat.fem$Hyper.p <- factor(graph.RF_Plat.fem$Hyper.p,
                                   levels = graph.RF_Plat.fem$Hyper.p[order(as.numeric(str_extract(graph.RF_Plat.fem$Hyper.p, "[0-9]+")))])
graph.RF_Plat.male$Hyper.p <- factor(graph.RF_Plat.male$Hyper.p,
                                    levels = graph.RF_Plat.male$Hyper.p[order(as.numeric(str_extract(graph.RF_Plat.male$Hyper.p, "[0-9]+")))])

plot.RF.Plat.fem.test <- ggplot(graph.RF_Plat.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.Plat.fem.train <- ggplot(graph.RF_Plat.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.Plat.fem.comb <- plot.RF.Plat.fem.train + plot.RF.Plat.fem.test + 
  plot_annotation(title = "RF-Plat Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.Plat.fem.comb)

plot.RF.Plat.male.test <- ggplot(graph.RF_Plat.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.Plat.male.train <- ggplot(graph.RF_Plat.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.Plat.male.comb <- plot.RF.Plat.male.train + plot.RF.Plat.male.test + 
  plot_annotation(title = "RF-Plat Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.Plat.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.RF_Plat.fem) {
  keep <- rep(TRUE, nrow(graph.RF_Plat.fem))
  for (i in 1:nrow(graph.RF_Plat.fem)) {
    for (j in 1:nrow(graph.RF_Plat.fem)) {
      if (i != j) {
        if (graph.RF_Plat.fem$rmse.train[j] <= graph.RF_Plat.fem$rmse.train[i] &&
            graph.RF_Plat.fem$rmse.test[j]  <= graph.RF_Plat.fem$rmse.test[i] &&
            (graph.RF_Plat.fem$rmse.train[j] < graph.RF_Plat.fem$rmse.train[i] ||
             graph.RF_Plat.fem$rmse.test[j]  < graph.RF_Plat.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_Plat.fem$pareto <- pareto_front(graph.RF_Plat.fem)

# Plot
Plot.EFF.RF.Plat.fem <- ggplot(graph.RF_Plat.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_Plat.fem[graph.RF_Plat.fem$pareto, ][order(graph.RF_Plat.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_Plat.fem[graph.RF_Plat.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-Plat Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.RF_Plat.male) {
  keep <- rep(TRUE, nrow(graph.RF_Plat.male))
  for (i in 1:nrow(graph.RF_Plat.male)) {
    for (j in 1:nrow(graph.RF_Plat.male)) {
      if (i != j) {
        if (graph.RF_Plat.male$rmse.train[j] <= graph.RF_Plat.male$rmse.train[i] &&
            graph.RF_Plat.male$rmse.test[j]  <= graph.RF_Plat.male$rmse.test[i] &&
            (graph.RF_Plat.male$rmse.train[j] < graph.RF_Plat.male$rmse.train[i] ||
             graph.RF_Plat.male$rmse.test[j]  < graph.RF_Plat.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_Plat.male$pareto <- pareto_front(graph.RF_Plat.male)

# Plot
Plot.EFF.RF.Plat.male <- ggplot(graph.RF_Plat.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_Plat.male[graph.RF_Plat.male$pareto, ][order(graph.RF_Plat.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_Plat.male[graph.RF_Plat.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-Plat Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.RF.Plat.comb <- Plot.EFF.RF.Plat.fem + Plot.EFF.RF.Plat.male 
print(Plot.EFF.RF.Plat.comb)
