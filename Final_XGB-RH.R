
Deaths.test.XGB.RH <- Deaths.test
Deaths.test.XGB.RH$psi.RH <- 0

# XGB algorithm does not work with factor variable
data_list <- list(Deaths.RH, Deaths.test.XGB.RH)
data_list_encoded <- lapply(data_list, function(dummy) {
  createDummyFeatures(dummy, target = "psi.RH")
})
Deaths.XGB.RH <- data_list_encoded[[1]]
Deaths.test.XGB.RH <- data_list_encoded[[2]]

# Creating learner
xgb <- makeLearner("regr.xgboost", predict.type = "response")

# Creating training and testing tasks
trainTask.XGB.RH <- makeRegrTask(data = Deaths.XGB.RH, target = "psi.RH")
testTask.XGB.RH <- makeRegrTask(data = Deaths.test.XGB.RH, target = "psi.RH")

# Creating parameters grid
xgb_params <- expand.grid(
  nrounds = c(50, 100, 150),
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_XGB.RH.train <- Deaths.RH
output_XGB.RH.test <- Deaths.test.XGB.RH

# Calibration of psi over all hyper-parameter combinations
for (i in 1:nrow(xgb_params)) {
  hp.xgb <- xgb_params[i, ]
  
  # Selecting hyper-parameter set
  pars.xgb <- setHyperPars(
    learner = xgb,
    par.vals = list(
      nrounds = hp.xgb$nrounds,
      eta = hp.xgb$eta,
      max_depth = hp.xgb$max_depth
    )
  )
  
  # Cross-validation
  resample_result <- resample(
    learner = pars.xgb,
    task = trainTask.XGB.RH,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.xgb <- train(pars.xgb, trainTask.XGB.RH)
  
  # Prediction of the model
  preds.test.xgb <- predict(model.xgb, task = testTask.XGB.RH)
  preds.train.xgb <- predict(model.xgb, task = trainTask.XGB.RH)
  
  # Saving predictions
  output_XGB.RH.test[[paste0("hp ",i)]] <- preds.test.xgb$data$response
  output_XGB.RH.train[[paste0("hp ",i)]] <- preds.train.xgb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.RH <- as.vector(as.matrix(for.m.RH.fem))
Rate.test.male.RH <- as.vector(as.matrix(for.m.RH.male))

test.psi.fem.XGB.RH <- output_XGB.RH.test %>% filter(Gender.f == 1 & Gender.m == 0)
test.psi.fem.XGB.RH <- test.psi.fem.XGB.RH[-1:-6]
test.psi.male.XGB.RH <- output_XGB.RH.test %>% filter(Gender.f == 0 & Gender.m == 1)
test.psi.male.XGB.RH <- test.psi.male.XGB.RH[-1:-6]

test.imp.rates.fem.XGB.RH <- as.data.frame(as.matrix(test.psi.fem.XGB.RH)*Rate.test.fem.RH)
test.imp.rates.fem.XGB.RH$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.XGB.RH <- as.data.frame(as.matrix(test.psi.male.XGB.RH)*Rate.test.male.RH)
test.imp.rates.male.XGB.RH$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.XGB.RH.fem <- c()
test.rmse_results.XGB.RH.male <- c()

for (i in colnames(test.imp.rates.fem.XGB.RH)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.RH.fem <- RMSE(test.imp.rates.fem.XGB.RH[[i]], test.imp.rates.fem.XGB.RH$obs.test)
    test.rmse_results.XGB.RH.fem[i] <- test.rmse_val.XGB.RH.fem
  }
}

for (i in colnames(test.imp.rates.male.XGB.RH)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.RH.male <- RMSE(test.imp.rates.male.XGB.RH[[i]], test.imp.rates.male.XGB.RH$obs.test)
    test.rmse_results.XGB.RH.male[i] <- test.rmse_val.XGB.RH.male
  }
}

test.rmse.XGB.RH.fem <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.RH.fem),
  rmse.test = as.numeric(test.rmse_results.XGB.RH.fem)
)
test.rmse.XGB.RH.male <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.RH.male),
  rmse.test = as.numeric(test.rmse_results.XGB.RH.male)
)

test.rmse.lower.XGB.RH.fem <- test.rmse.XGB.RH.fem %>% filter(rmse.test < rmse.for.RH.fem)
test.rmse.lower.XGB.RH.male <- test.rmse.XGB.RH.male %>% filter(rmse.test < rmse.for.RH.male)

Rate.train.fem.RH <- as.vector(as.matrix(m.RH.fem))
Rate.train.male.RH <- as.vector(as.matrix(m.RH.male))

train.psi.fem.XGB.RH <- output_XGB.RH.train %>% filter(Gender == "f")
train.psi.fem.XGB.RH <- train.psi.fem.XGB.RH[-1:-5]
train.psi.male.XGB.RH <- output_XGB.RH.train %>% filter(Gender == "m")
train.psi.male.XGB.RH <- train.psi.male.XGB.RH[-1:-5]

train.imp.rates.fem.XGB.RH <- as.data.frame(as.matrix(train.psi.fem.XGB.RH)*Rate.train.fem.RH)
train.imp.rates.fem.XGB.RH$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.XGB.RH <- as.data.frame(as.matrix(train.psi.male.XGB.RH)*Rate.train.male.RH)
train.imp.rates.male.XGB.RH$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.XGB.RH.fem <- c()
train.rmse_results.XGB.RH.male <- c()

for (i in colnames(train.imp.rates.fem.XGB.RH)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.RH.fem <- RMSE(train.imp.rates.fem.XGB.RH[[i]], train.imp.rates.fem.XGB.RH$obs.train)
    train.rmse_results.XGB.RH.fem[i] <- train.rmse_val.XGB.RH.fem
  }
}
for (i in colnames(train.imp.rates.male.XGB.RH)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.RH.male <- RMSE(train.imp.rates.male.XGB.RH[[i]], train.imp.rates.male.XGB.RH$obs.train)
    train.rmse_results.XGB.RH.male[i] <- train.rmse_val.XGB.RH.male
  }
}
train.rmse.XGB.RH.fem <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.RH.fem),
  rmse.train = as.numeric(train.rmse_results.XGB.RH.fem))
train.rmse.XGB.RH.male <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.RH.male),
  rmse.train = as.numeric(train.rmse_results.XGB.RH.male))

train.rmse.lower.XGB.RH.fem <- train.rmse.XGB.RH.fem %>% filter(rmse.train < rmse.fit.RH.fem)
tt.rmse.XGB.RH.fem <- inner_join(test.rmse.lower.XGB.RH.fem, train.rmse.lower.XGB.RH.fem, by = "Hyper.p")
train.rmse.lower.XGB.RH.male <- train.rmse.XGB.RH.male %>% filter(rmse.train < rmse.fit.RH.male)
tt.rmse.XGB.RH.male <- inner_join(test.rmse.lower.XGB.RH.male, train.rmse.lower.XGB.RH.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.XGB.RH.fem$rmse.test <- round(tt.rmse.XGB.RH.fem$rmse.test, digits = 8)
tt.rmse.XGB.RH.fem$rmse.train <- round(tt.rmse.XGB.RH.fem$rmse.train, digits = 8)
tt.rmse.XGB.RH.male$rmse.test <- round(tt.rmse.XGB.RH.male$rmse.test, digits = 8)
tt.rmse.XGB.RH.male$rmse.train <- round(tt.rmse.XGB.RH.male$rmse.train, digits = 8)

graph.XGB_RH.fem <- tt.rmse.XGB.RH.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.XGB_RH.male <- tt.rmse.XGB.RH.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.XGB_RH.fem <- graph.XGB_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.XGB_RH.male <- graph.XGB_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.XGB_RH.fem <- graph.XGB_RH.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.XGB_RH.male <- graph.XGB_RH.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.XGB_RH.fem <- graph.XGB_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.XGB_RH.male <- graph.XGB_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.XGB_RH.fem$Hyper.p <- factor(graph.XGB_RH.fem$Hyper.p,
                                    levels = graph.XGB_RH.fem$Hyper.p[order(as.numeric(str_extract(graph.XGB_RH.fem$Hyper.p, "[0-9]+")))])
graph.XGB_RH.male$Hyper.p <- factor(graph.XGB_RH.male$Hyper.p,
                                     levels = graph.XGB_RH.male$Hyper.p[order(as.numeric(str_extract(graph.XGB_RH.male$Hyper.p, "[0-9]+")))])

plot.XGB.RH.fem.test <- ggplot(graph.XGB_RH.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.RH.fem.train <- ggplot(graph.XGB_RH.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.RH.fem.comb <- plot.XGB.RH.fem.train + plot.XGB.RH.fem.test + 
  plot_annotation(title = "XGB-RH Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.RH.fem.comb)

plot.XGB.RH.male.test <- ggplot(graph.XGB_RH.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.RH.male.train <- ggplot(graph.XGB_RH.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.RH.male.comb <- plot.XGB.RH.male.train + plot.XGB.RH.male.test + 
  plot_annotation(title = "XGB-RH Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.RH.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.XGB_RH.fem) {
  keep <- rep(TRUE, nrow(graph.XGB_RH.fem))
  for (i in 1:nrow(graph.XGB_RH.fem)) {
    for (j in 1:nrow(graph.XGB_RH.fem)) {
      if (i != j) {
        if (graph.XGB_RH.fem$rmse.train[j] <= graph.XGB_RH.fem$rmse.train[i] &&
            graph.XGB_RH.fem$rmse.test[j]  <= graph.XGB_RH.fem$rmse.test[i] &&
            (graph.XGB_RH.fem$rmse.train[j] < graph.XGB_RH.fem$rmse.train[i] ||
             graph.XGB_RH.fem$rmse.test[j]  < graph.XGB_RH.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_RH.fem$pareto <- pareto_front(graph.XGB_RH.fem)

# Plot
Plot.EFF.XGB.RH.fem <- ggplot(graph.XGB_RH.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_RH.fem[graph.XGB_RH.fem$pareto, ][order(graph.XGB_RH.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_RH.fem[graph.XGB_RH.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-RH Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.XGB_RH.male) {
  keep <- rep(TRUE, nrow(graph.XGB_RH.male))
  for (i in 1:nrow(graph.XGB_RH.male)) {
    for (j in 1:nrow(graph.XGB_RH.male)) {
      if (i != j) {
        if (graph.XGB_RH.male$rmse.train[j] <= graph.XGB_RH.male$rmse.train[i] &&
            graph.XGB_RH.male$rmse.test[j]  <= graph.XGB_RH.male$rmse.test[i] &&
            (graph.XGB_RH.male$rmse.train[j] < graph.XGB_RH.male$rmse.train[i] ||
             graph.XGB_RH.male$rmse.test[j]  < graph.XGB_RH.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_RH.male$pareto <- pareto_front(graph.XGB_RH.male)

# Plot
Plot.EFF.XGB.RH.male <- ggplot(graph.XGB_RH.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_RH.male[graph.XGB_RH.male$pareto, ][order(graph.XGB_RH.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_RH.male[graph.XGB_RH.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-RH Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.XGB.RH.comb <- Plot.EFF.XGB.RH.fem + Plot.EFF.XGB.RH.male 
print(Plot.EFF.XGB.RH.comb)
