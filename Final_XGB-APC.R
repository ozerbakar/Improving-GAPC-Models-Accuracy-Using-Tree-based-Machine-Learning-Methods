
Deaths.test.XGB.APC <- Deaths.test
Deaths.test.XGB.APC$psi.APC <- 0

# XGB algorithm does not work with factor variable
data_list <- list(Deaths.APC, Deaths.test.XGB.APC)
data_list_encoded <- lapply(data_list, function(dummy) {
  createDummyFeatures(dummy, target = "psi.APC")
})
Deaths.XGB.APC <- data_list_encoded[[1]]
Deaths.test.XGB.APC <- data_list_encoded[[2]]

# Creating learner
xgb <- makeLearner("regr.xgboost", predict.type = "response")

# Creating training and testing tasks
trainTask.XGB.APC <- makeRegrTask(data = Deaths.XGB.APC, target = "psi.APC")
testTask.XGB.APC <- makeRegrTask(data = Deaths.test.XGB.APC, target = "psi.APC")

# Creating parameters grid
xgb_params <- expand.grid(
  nrounds = c(50, 100, 150),
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_XGB.APC.train <- Deaths.APC
output_XGB.APC.test <- Deaths.test.XGB.APC

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
    task = trainTask.XGB.APC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.xgb <- train(pars.xgb, trainTask.XGB.APC)
  
  # Prediction of the model
  preds.test.xgb <- predict(model.xgb, task = testTask.XGB.APC)
  preds.train.xgb <- predict(model.xgb, task = trainTask.XGB.APC)
  
  # Saving predictions
  output_XGB.APC.test[[paste0("hp ",i)]] <- preds.test.xgb$data$response
  output_XGB.APC.train[[paste0("hp ",i)]] <- preds.train.xgb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.APC <- as.vector(as.matrix(for.m.APC.fem))
Rate.test.male.APC <- as.vector(as.matrix(for.m.APC.male))

test.psi.fem.XGB.APC <- output_XGB.APC.test %>% filter(Gender.f == 1 & Gender.m == 0)
test.psi.fem.XGB.APC <- test.psi.fem.XGB.APC[-1:-6]
test.psi.male.XGB.APC <- output_XGB.APC.test %>% filter(Gender.f == 0 & Gender.m == 1)
test.psi.male.XGB.APC <- test.psi.male.XGB.APC[-1:-6]

test.imp.rates.fem.XGB.APC <- as.data.frame(as.matrix(test.psi.fem.XGB.APC)*Rate.test.fem.APC)
test.imp.rates.fem.XGB.APC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.XGB.APC <- as.data.frame(as.matrix(test.psi.male.XGB.APC)*Rate.test.male.APC)
test.imp.rates.male.XGB.APC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.XGB.APC.fem <- c()
test.rmse_results.XGB.APC.male <- c()

for (i in colnames(test.imp.rates.fem.XGB.APC)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.APC.fem <- RMSE(test.imp.rates.fem.XGB.APC[[i]], test.imp.rates.fem.XGB.APC$obs.test)
    test.rmse_results.XGB.APC.fem[i] <- test.rmse_val.XGB.APC.fem
  }
}

for (i in colnames(test.imp.rates.male.XGB.APC)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.APC.male <- RMSE(test.imp.rates.male.XGB.APC[[i]], test.imp.rates.male.XGB.APC$obs.test)
    test.rmse_results.XGB.APC.male[i] <- test.rmse_val.XGB.APC.male
  }
}

test.rmse.XGB.APC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.APC.fem),
  rmse.test = as.numeric(test.rmse_results.XGB.APC.fem)
)
test.rmse.XGB.APC.male <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.APC.male),
  rmse.test = as.numeric(test.rmse_results.XGB.APC.male)
)

test.rmse.lower.XGB.APC.fem <- test.rmse.XGB.APC.fem %>% filter(rmse.test < rmse.for.APC.fem)
test.rmse.lower.XGB.APC.male <- test.rmse.XGB.APC.male %>% filter(rmse.test < rmse.for.APC.male)

Rate.train.fem.APC <- as.vector(as.matrix(m.APC.fem))
Rate.train.male.APC <- as.vector(as.matrix(m.APC.male))

train.psi.fem.XGB.APC <- output_XGB.APC.train %>% filter(Gender == "f")
train.psi.fem.XGB.APC <- train.psi.fem.XGB.APC[-1:-5]
train.psi.male.XGB.APC <- output_XGB.APC.train %>% filter(Gender == "m")
train.psi.male.XGB.APC <- train.psi.male.XGB.APC[-1:-5]

train.imp.rates.fem.XGB.APC <- as.data.frame(as.matrix(train.psi.fem.XGB.APC)*Rate.train.fem.APC)
train.imp.rates.fem.XGB.APC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.XGB.APC <- as.data.frame(as.matrix(train.psi.male.XGB.APC)*Rate.train.male.APC)
train.imp.rates.male.XGB.APC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.XGB.APC.fem <- c()
train.rmse_results.XGB.APC.male <- c()

for (i in colnames(train.imp.rates.fem.XGB.APC)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.APC.fem <- RMSE(train.imp.rates.fem.XGB.APC[[i]], train.imp.rates.fem.XGB.APC$obs.train)
    train.rmse_results.XGB.APC.fem[i] <- train.rmse_val.XGB.APC.fem
  }
}
for (i in colnames(train.imp.rates.male.XGB.APC)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.APC.male <- RMSE(train.imp.rates.male.XGB.APC[[i]], train.imp.rates.male.XGB.APC$obs.train)
    train.rmse_results.XGB.APC.male[i] <- train.rmse_val.XGB.APC.male
  }
}
train.rmse.XGB.APC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.APC.fem),
  rmse.train = as.numeric(train.rmse_results.XGB.APC.fem))
train.rmse.XGB.APC.male <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.APC.male),
  rmse.train = as.numeric(train.rmse_results.XGB.APC.male))

train.rmse.lower.XGB.APC.fem <- train.rmse.XGB.APC.fem %>% filter(rmse.train < rmse.fit.APC.fem)
tt.rmse.XGB.APC.fem <- inner_join(test.rmse.lower.XGB.APC.fem, train.rmse.lower.XGB.APC.fem, by = "Hyper.p")
train.rmse.lower.XGB.APC.male <- train.rmse.XGB.APC.male %>% filter(rmse.train < rmse.fit.APC.male)
tt.rmse.XGB.APC.male <- inner_join(test.rmse.lower.XGB.APC.male, train.rmse.lower.XGB.APC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.XGB.APC.fem$rmse.test <- round(tt.rmse.XGB.APC.fem$rmse.test, digits = 8)
tt.rmse.XGB.APC.fem$rmse.train <- round(tt.rmse.XGB.APC.fem$rmse.train, digits = 8)
tt.rmse.XGB.APC.male$rmse.test <- round(tt.rmse.XGB.APC.male$rmse.test, digits = 8)
tt.rmse.XGB.APC.male$rmse.train <- round(tt.rmse.XGB.APC.male$rmse.train, digits = 8)

graph.XGB_APC.fem <- tt.rmse.XGB.APC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.XGB_APC.male <- tt.rmse.XGB.APC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.XGB_APC.fem <- graph.XGB_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.XGB_APC.male <- graph.XGB_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.XGB_APC.fem <- graph.XGB_APC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.XGB_APC.male <- graph.XGB_APC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.XGB_APC.fem <- graph.XGB_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.XGB_APC.male <- graph.XGB_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.XGB_APC.fem$Hyper.p <- factor(graph.XGB_APC.fem$Hyper.p,
                                    levels = graph.XGB_APC.fem$Hyper.p[order(as.numeric(str_extract(graph.XGB_APC.fem$Hyper.p, "[0-9]+")))])
graph.XGB_APC.male$Hyper.p <- factor(graph.XGB_APC.male$Hyper.p,
                                     levels = graph.XGB_APC.male$Hyper.p[order(as.numeric(str_extract(graph.XGB_APC.male$Hyper.p, "[0-9]+")))])

plot.XGB.APC.fem.test <- ggplot(graph.XGB_APC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.APC.fem.train <- ggplot(graph.XGB_APC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.APC.fem.comb <- plot.XGB.APC.fem.train + plot.XGB.APC.fem.test + 
  plot_annotation(title = "XGB-APC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.APC.fem.comb)

plot.XGB.APC.male.test <- ggplot(graph.XGB_APC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.APC.male.train <- ggplot(graph.XGB_APC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.APC.male.comb <- plot.XGB.APC.male.train + plot.XGB.APC.male.test + 
  plot_annotation(title = "XGB-APC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.APC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.XGB_APC.fem) {
  keep <- rep(TRUE, nrow(graph.XGB_APC.fem))
  for (i in 1:nrow(graph.XGB_APC.fem)) {
    for (j in 1:nrow(graph.XGB_APC.fem)) {
      if (i != j) {
        if (graph.XGB_APC.fem$rmse.train[j] <= graph.XGB_APC.fem$rmse.train[i] &&
            graph.XGB_APC.fem$rmse.test[j]  <= graph.XGB_APC.fem$rmse.test[i] &&
            (graph.XGB_APC.fem$rmse.train[j] < graph.XGB_APC.fem$rmse.train[i] ||
             graph.XGB_APC.fem$rmse.test[j]  < graph.XGB_APC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_APC.fem$pareto <- pareto_front(graph.XGB_APC.fem)

# Plot
Plot.EFF.XGB.APC.fem <- ggplot(graph.XGB_APC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_APC.fem[graph.XGB_APC.fem$pareto, ][order(graph.XGB_APC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_APC.fem[graph.XGB_APC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-APC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.XGB_APC.male) {
  keep <- rep(TRUE, nrow(graph.XGB_APC.male))
  for (i in 1:nrow(graph.XGB_APC.male)) {
    for (j in 1:nrow(graph.XGB_APC.male)) {
      if (i != j) {
        if (graph.XGB_APC.male$rmse.train[j] <= graph.XGB_APC.male$rmse.train[i] &&
            graph.XGB_APC.male$rmse.test[j]  <= graph.XGB_APC.male$rmse.test[i] &&
            (graph.XGB_APC.male$rmse.train[j] < graph.XGB_APC.male$rmse.train[i] ||
             graph.XGB_APC.male$rmse.test[j]  < graph.XGB_APC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_APC.male$pareto <- pareto_front(graph.XGB_APC.male)

# Plot
Plot.EFF.XGB.APC.male <- ggplot(graph.XGB_APC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_APC.male[graph.XGB_APC.male$pareto, ][order(graph.XGB_APC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_APC.male[graph.XGB_APC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-APC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.XGB.APC.comb <- Plot.EFF.XGB.APC.fem + Plot.EFF.XGB.APC.male 
print(Plot.EFF.XGB.APC.comb)
