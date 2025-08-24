
Deaths.test.RF.CBD <- Deaths.test
Deaths.test.RF.CBD$psi.CBD <- 0

# Creating learner
rf <- makeLearner("regr.ranger", predict.type = "response")

# Creating training and testing tasks
trainTask.RF.CBD <- makeRegrTask(data = Deaths.CBD, target = "psi.CBD")
testTask.RF.CBD <- makeRegrTask(data = Deaths.test.RF.CBD, target = "psi.CBD")

# Creating parameters grid
rf_params <- expand.grid(
  mtry = c(1,2, 3, 4),
  num.trees = c(50, 100, 150, 200, 300),
  min.node.size = c(1:5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_RF.CBD.train <- Deaths.CBD
output_RF.CBD.test <- Deaths.test.RF.CBD

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
    task = trainTask.RF.CBD,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.rf <- train(pars.rf, trainTask.RF.CBD)
  
  # Prediction of the model
  preds.test.rf <- predict(model.rf, task = testTask.RF.CBD)
  preds.train.rf <- predict(model.rf, task = trainTask.RF.CBD)
  
  # Saving predictions
  output_RF.CBD.test[[paste0("hp ",i)]] <- preds.test.rf$data$response
  output_RF.CBD.train[[paste0("hp ",i)]] <- preds.train.rf$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.CBD <- as.vector(as.matrix(for.m.CBD.fem))
Rate.test.male.CBD <- as.vector(as.matrix(for.m.CBD.male))

test.psi.fem.RF.CBD <- output_RF.CBD.test %>% filter(Gender == "f")
test.psi.fem.RF.CBD <- test.psi.fem.RF.CBD[-1:-5]
test.psi.male.RF.CBD <- output_RF.CBD.test %>% filter(Gender == "m")
test.psi.male.RF.CBD <- test.psi.male.RF.CBD[-1:-5]

test.imp.rates.fem.RF.CBD <- as.data.frame(as.matrix(test.psi.fem.RF.CBD)*Rate.test.fem.CBD)
test.imp.rates.fem.RF.CBD$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.RF.CBD <- as.data.frame(as.matrix(test.psi.male.RF.CBD)*Rate.test.male.CBD)
test.imp.rates.male.RF.CBD$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.RF.CBD.fem <- c()
test.rmse_results.RF.CBD.male <- c()

for (i in colnames(test.imp.rates.fem.RF.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.RF.CBD.fem <- RMSE(test.imp.rates.fem.RF.CBD[[i]], test.imp.rates.fem.RF.CBD$obs.test)
    test.rmse_results.RF.CBD.fem[i] <- test.rmse_val.RF.CBD.fem
  }
}

for (i in colnames(test.imp.rates.male.RF.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.RF.CBD.male <- RMSE(test.imp.rates.male.RF.CBD[[i]], test.imp.rates.male.RF.CBD$obs.test)
    test.rmse_results.RF.CBD.male[i] <- test.rmse_val.RF.CBD.male
  }
}

test.rmse.RF.CBD.fem <- data.frame(
  Hyper.p = names(test.rmse_results.RF.CBD.fem),
  rmse.test = as.numeric(test.rmse_results.RF.CBD.fem)
)
test.rmse.RF.CBD.male <- data.frame(
  Hyper.p = names(test.rmse_results.RF.CBD.male),
  rmse.test = as.numeric(test.rmse_results.RF.CBD.male)
)

test.rmse.lower.RF.CBD.fem <- test.rmse.RF.CBD.fem %>% filter(rmse.test < rmse.for.CBD.fem)
test.rmse.lower.RF.CBD.male <- test.rmse.RF.CBD.male %>% filter(rmse.test < rmse.for.CBD.male)

Rate.train.fem.CBD <- as.vector(as.matrix(m.CBD.fem))
Rate.train.male.CBD <- as.vector(as.matrix(m.CBD.male))

train.psi.fem.RF.CBD <- output_RF.CBD.train %>% filter(Gender == "f")
train.psi.fem.RF.CBD <- train.psi.fem.RF.CBD[-1:-5]
train.psi.male.RF.CBD <- output_RF.CBD.train %>% filter(Gender == "m")
train.psi.male.RF.CBD <- train.psi.male.RF.CBD[-1:-5]

train.imp.rates.fem.RF.CBD <- as.data.frame(as.matrix(train.psi.fem.RF.CBD)*Rate.train.fem.CBD)
train.imp.rates.fem.RF.CBD$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.RF.CBD <- as.data.frame(as.matrix(train.psi.male.RF.CBD)*Rate.train.male.CBD)
train.imp.rates.male.RF.CBD$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.RF.CBD.fem <- c()
train.rmse_results.RF.CBD.male <- c()

for (i in colnames(train.imp.rates.fem.RF.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.RF.CBD.fem <- RMSE(train.imp.rates.fem.RF.CBD[[i]], train.imp.rates.fem.RF.CBD$obs.train)
    train.rmse_results.RF.CBD.fem[i] <- train.rmse_val.RF.CBD.fem
  }
}
for (i in colnames(train.imp.rates.male.RF.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.RF.CBD.male <- RMSE(train.imp.rates.male.RF.CBD[[i]], train.imp.rates.male.RF.CBD$obs.train)
    train.rmse_results.RF.CBD.male[i] <- train.rmse_val.RF.CBD.male
  }
}
train.rmse.RF.CBD.fem <- data.frame(
  Hyper.p = names(train.rmse_results.RF.CBD.fem),
  rmse.train = as.numeric(train.rmse_results.RF.CBD.fem))
train.rmse.RF.CBD.male <- data.frame(
  Hyper.p = names(train.rmse_results.RF.CBD.male),
  rmse.train = as.numeric(train.rmse_results.RF.CBD.male))

train.rmse.lower.RF.CBD.fem <- train.rmse.RF.CBD.fem %>% filter(rmse.train < rmse.fit.CBD.fem)
tt.rmse.RF.CBD.fem <- inner_join(test.rmse.lower.RF.CBD.fem, train.rmse.lower.RF.CBD.fem, by = "Hyper.p")
train.rmse.lower.RF.CBD.male <- train.rmse.RF.CBD.male %>% filter(rmse.train < rmse.fit.CBD.male)
tt.rmse.RF.CBD.male <- inner_join(test.rmse.lower.RF.CBD.male, train.rmse.lower.RF.CBD.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.RF.CBD.fem$rmse.test <- round(tt.rmse.RF.CBD.fem$rmse.test, digits = 8)
tt.rmse.RF.CBD.fem$rmse.train <- round(tt.rmse.RF.CBD.fem$rmse.train, digits = 8)
tt.rmse.RF.CBD.male$rmse.test <- round(tt.rmse.RF.CBD.male$rmse.test, digits = 8)
tt.rmse.RF.CBD.male$rmse.train <- round(tt.rmse.RF.CBD.male$rmse.train, digits = 8)

graph.RF_CBD.fem <- tt.rmse.RF.CBD.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.RF_CBD.male <- tt.rmse.RF.CBD.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.RF_CBD.fem <- graph.RF_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.RF_CBD.male <- graph.RF_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.RF_CBD.fem <- graph.RF_CBD.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.RF_CBD.male <- graph.RF_CBD.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.RF_CBD.fem <- graph.RF_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.RF_CBD.male <- graph.RF_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.RF_CBD.fem$Hyper.p <- factor(graph.RF_CBD.fem$Hyper.p,
                                  levels = graph.RF_CBD.fem$Hyper.p[order(as.numeric(str_extract(graph.RF_CBD.fem$Hyper.p, "[0-9]+")))])
graph.RF_CBD.male$Hyper.p <- factor(graph.RF_CBD.male$Hyper.p,
                                   levels = graph.RF_CBD.male$Hyper.p[order(as.numeric(str_extract(graph.RF_CBD.male$Hyper.p, "[0-9]+")))])

plot.RF.CBD.fem.test <- ggplot(graph.RF_CBD.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.CBD.fem.train <- ggplot(graph.RF_CBD.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.CBD.fem.comb <- plot.RF.CBD.fem.train + plot.RF.CBD.fem.test + 
  plot_annotation(title = "RF-CBD Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.CBD.fem.comb)

plot.RF.CBD.male.test <- ggplot(graph.RF_CBD.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.RF.CBD.male.train <- ggplot(graph.RF_CBD.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.RF.CBD.male.comb <- plot.RF.CBD.male.train + plot.RF.CBD.male.test + 
  plot_annotation(title = "RF-CBD Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.RF.CBD.male.comb)

# Pareto optimal function
pareto_front <- function(graph.RF_CBD.fem) {
  keep <- rep(TRUE, nrow(graph.RF_CBD.fem))
  for (i in 1:nrow(graph.RF_CBD.fem)) {
    for (j in 1:nrow(graph.RF_CBD.fem)) {
      if (i != j) {
        if (graph.RF_CBD.fem$rmse.train[j] <= graph.RF_CBD.fem$rmse.train[i] &&
            graph.RF_CBD.fem$rmse.test[j]  <= graph.RF_CBD.fem$rmse.test[i] &&
            (graph.RF_CBD.fem$rmse.train[j] < graph.RF_CBD.fem$rmse.train[i] ||
             graph.RF_CBD.fem$rmse.test[j]  < graph.RF_CBD.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_CBD.fem$pareto <- pareto_front(graph.RF_CBD.fem)

# Plot
library(ggplot2)
ggplot(graph.RF_CBD.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_CBD.fem[graph.RF_CBD.fem$pareto, ][order(graph.RF_CBD.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_CBD.fem[graph.RF_CBD.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "Efficient Frontier",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.RF_CBD.fem) {
  keep <- rep(TRUE, nrow(graph.RF_CBD.fem))
  for (i in 1:nrow(graph.RF_CBD.fem)) {
    for (j in 1:nrow(graph.RF_CBD.fem)) {
      if (i != j) {
        if (graph.RF_CBD.fem$rmse.train[j] <= graph.RF_CBD.fem$rmse.train[i] &&
            graph.RF_CBD.fem$rmse.test[j]  <= graph.RF_CBD.fem$rmse.test[i] &&
            (graph.RF_CBD.fem$rmse.train[j] < graph.RF_CBD.fem$rmse.train[i] ||
             graph.RF_CBD.fem$rmse.test[j]  < graph.RF_CBD.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_CBD.fem$pareto <- pareto_front(graph.RF_CBD.fem)

# Plot
Plot.EFF.RF.CBD.fem <- ggplot(graph.RF_CBD.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_CBD.fem[graph.RF_CBD.fem$pareto, ][order(graph.RF_CBD.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_CBD.fem[graph.RF_CBD.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-CBD Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.RF_CBD.male) {
  keep <- rep(TRUE, nrow(graph.RF_CBD.male))
  for (i in 1:nrow(graph.RF_CBD.male)) {
    for (j in 1:nrow(graph.RF_CBD.male)) {
      if (i != j) {
        if (graph.RF_CBD.male$rmse.train[j] <= graph.RF_CBD.male$rmse.train[i] &&
            graph.RF_CBD.male$rmse.test[j]  <= graph.RF_CBD.male$rmse.test[i] &&
            (graph.RF_CBD.male$rmse.train[j] < graph.RF_CBD.male$rmse.train[i] ||
             graph.RF_CBD.male$rmse.test[j]  < graph.RF_CBD.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.RF_CBD.male$pareto <- pareto_front(graph.RF_CBD.male)

# Plot

Plot.EFF.RF.CBD.male <- ggplot(graph.RF_CBD.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.RF_CBD.male[graph.RF_CBD.male$pareto, ][order(graph.RF_CBD.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.RF_CBD.male[graph.RF_CBD.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "RF-CBD Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.RF.CBD.comb <- Plot.EFF.RF.CBD.fem + Plot.EFF.RF.CBD.male 
print(Plot.EFF.RF.CBD.comb)
