
Deaths.test.XGB.CBD <- Deaths.test
Deaths.test.XGB.CBD$psi.CBD <- 0

# XGB algorithm does not work with factor variable
data_list <- list(Deaths.CBD, Deaths.test.XGB.CBD)
data_list_encoded <- lapply(data_list, function(dummy) {
  createDummyFeatures(dummy, target = "psi.CBD")
})
Deaths.XGB.CBD <- data_list_encoded[[1]]
Deaths.test.XGB.CBD <- data_list_encoded[[2]]

# Creating learner
xgb <- makeLearner("regr.xgboost", predict.type = "response")

# Creating training and testing tasks
trainTask.XGB.CBD <- makeRegrTask(data = Deaths.XGB.CBD, target = "psi.CBD")
testTask.XGB.CBD <- makeRegrTask(data = Deaths.test.XGB.CBD, target = "psi.CBD")

# Creating parameters grid
xgb_params <- expand.grid(
  nrounds = c(50, 100, 150),
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_XGB.CBD.train <- Deaths.CBD
output_XGB.CBD.test <- Deaths.test.XGB.CBD

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
    task = trainTask.XGB.CBD,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.xgb <- train(pars.xgb, trainTask.XGB.CBD)
  
  # Prediction of the model
  preds.test.xgb <- predict(model.xgb, task = testTask.XGB.CBD)
  preds.train.xgb <- predict(model.xgb, task = trainTask.XGB.CBD)
  
  # Saving predictions
  output_XGB.CBD.test[[paste0("hp ",i)]] <- preds.test.xgb$data$response
  output_XGB.CBD.train[[paste0("hp ",i)]] <- preds.train.xgb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.CBD <- as.vector(as.matrix(for.m.CBD.fem))
Rate.test.male.CBD <- as.vector(as.matrix(for.m.CBD.male))

test.psi.fem.XGB.CBD <- output_XGB.CBD.test %>% filter(Gender.f == 1 & Gender.m == 0)
test.psi.fem.XGB.CBD <- test.psi.fem.XGB.CBD[-1:-6]
test.psi.male.XGB.CBD <- output_XGB.CBD.test %>% filter(Gender.f == 0 & Gender.m == 1)
test.psi.male.XGB.CBD <- test.psi.male.XGB.CBD[-1:-6]

test.imp.rates.fem.XGB.CBD <- as.data.frame(as.matrix(test.psi.fem.XGB.CBD)*Rate.test.fem.CBD)
test.imp.rates.fem.XGB.CBD$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.XGB.CBD <- as.data.frame(as.matrix(test.psi.male.XGB.CBD)*Rate.test.male.CBD)
test.imp.rates.male.XGB.CBD$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.XGB.CBD.fem <- c()
test.rmse_results.XGB.CBD.male <- c()

for (i in colnames(test.imp.rates.fem.XGB.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.CBD.fem <- RMSE(test.imp.rates.fem.XGB.CBD[[i]], test.imp.rates.fem.XGB.CBD$obs.test)
    test.rmse_results.XGB.CBD.fem[i] <- test.rmse_val.XGB.CBD.fem
  }
}

for (i in colnames(test.imp.rates.male.XGB.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.CBD.male <- RMSE(test.imp.rates.male.XGB.CBD[[i]], test.imp.rates.male.XGB.CBD$obs.test)
    test.rmse_results.XGB.CBD.male[i] <- test.rmse_val.XGB.CBD.male
  }
}

test.rmse.XGB.CBD.fem <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.CBD.fem),
  rmse.test = as.numeric(test.rmse_results.XGB.CBD.fem)
)
test.rmse.XGB.CBD.male <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.CBD.male),
  rmse.test = as.numeric(test.rmse_results.XGB.CBD.male)
)

test.rmse.lower.XGB.CBD.fem <- test.rmse.XGB.CBD.fem %>% filter(rmse.test < rmse.for.CBD.fem)
test.rmse.lower.XGB.CBD.male <- test.rmse.XGB.CBD.male %>% filter(rmse.test < rmse.for.CBD.male)

Rate.train.fem.CBD <- as.vector(as.matrix(m.CBD.fem))
Rate.train.male.CBD <- as.vector(as.matrix(m.CBD.male))

train.psi.fem.XGB.CBD <- output_XGB.CBD.train %>% filter(Gender == "f")
train.psi.fem.XGB.CBD <- train.psi.fem.XGB.CBD[-1:-5]
train.psi.male.XGB.CBD <- output_XGB.CBD.train %>% filter(Gender == "m")
train.psi.male.XGB.CBD <- train.psi.male.XGB.CBD[-1:-5]

train.imp.rates.fem.XGB.CBD <- as.data.frame(as.matrix(train.psi.fem.XGB.CBD)*Rate.train.fem.CBD)
train.imp.rates.fem.XGB.CBD$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.XGB.CBD <- as.data.frame(as.matrix(train.psi.male.XGB.CBD)*Rate.train.male.CBD)
train.imp.rates.male.XGB.CBD$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.XGB.CBD.fem <- c()
train.rmse_results.XGB.CBD.male <- c()

for (i in colnames(train.imp.rates.fem.XGB.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.CBD.fem <- RMSE(train.imp.rates.fem.XGB.CBD[[i]], train.imp.rates.fem.XGB.CBD$obs.train)
    train.rmse_results.XGB.CBD.fem[i] <- train.rmse_val.XGB.CBD.fem
  }
}
for (i in colnames(train.imp.rates.male.XGB.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.CBD.male <- RMSE(train.imp.rates.male.XGB.CBD[[i]], train.imp.rates.male.XGB.CBD$obs.train)
    train.rmse_results.XGB.CBD.male[i] <- train.rmse_val.XGB.CBD.male
  }
}
train.rmse.XGB.CBD.fem <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.CBD.fem),
  rmse.train = as.numeric(train.rmse_results.XGB.CBD.fem))
train.rmse.XGB.CBD.male <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.CBD.male),
  rmse.train = as.numeric(train.rmse_results.XGB.CBD.male))

train.rmse.lower.XGB.CBD.fem <- train.rmse.XGB.CBD.fem %>% filter(rmse.train < rmse.fit.CBD.fem)
tt.rmse.XGB.CBD.fem <- inner_join(test.rmse.lower.XGB.CBD.fem, train.rmse.lower.XGB.CBD.fem, by = "Hyper.p")
train.rmse.lower.XGB.CBD.male <- train.rmse.XGB.CBD.male %>% filter(rmse.train < rmse.fit.CBD.male)
tt.rmse.XGB.CBD.male <- inner_join(test.rmse.lower.XGB.CBD.male, train.rmse.lower.XGB.CBD.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.XGB.CBD.fem$rmse.test <- round(tt.rmse.XGB.CBD.fem$rmse.test, digits = 8)
tt.rmse.XGB.CBD.fem$rmse.train <- round(tt.rmse.XGB.CBD.fem$rmse.train, digits = 8)
tt.rmse.XGB.CBD.male$rmse.test <- round(tt.rmse.XGB.CBD.male$rmse.test, digits = 8)
tt.rmse.XGB.CBD.male$rmse.train <- round(tt.rmse.XGB.CBD.male$rmse.train, digits = 8)

graph.XGB_CBD.fem <- tt.rmse.XGB.CBD.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.XGB_CBD.male <- tt.rmse.XGB.CBD.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.XGB_CBD.fem <- graph.XGB_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.XGB_CBD.male <- graph.XGB_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.XGB_CBD.fem <- graph.XGB_CBD.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.XGB_CBD.male <- graph.XGB_CBD.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.XGB_CBD.fem <- graph.XGB_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.XGB_CBD.male <- graph.XGB_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.XGB_CBD.fem$Hyper.p <- factor(graph.XGB_CBD.fem$Hyper.p,
                                   levels = graph.XGB_CBD.fem$Hyper.p[order(as.numeric(str_extract(graph.XGB_CBD.fem$Hyper.p, "[0-9]+")))])
graph.XGB_CBD.male$Hyper.p <- factor(graph.XGB_CBD.male$Hyper.p,
                                    levels = graph.XGB_CBD.male$Hyper.p[order(as.numeric(str_extract(graph.XGB_CBD.male$Hyper.p, "[0-9]+")))])

plot.XGB.CBD.fem.test <- ggplot(graph.XGB_CBD.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.CBD.fem.train <- ggplot(graph.XGB_CBD.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.CBD.fem.comb <- plot.XGB.CBD.fem.train + plot.XGB.CBD.fem.test + 
  plot_annotation(title = "XGB-CBD Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.CBD.fem.comb)

plot.XGB.CBD.male.test <- ggplot(graph.XGB_CBD.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.CBD.male.train <- ggplot(graph.XGB_CBD.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.CBD.male.comb <- plot.XGB.CBD.male.train + plot.XGB.CBD.male.test + 
  plot_annotation(title = "XGB-CBD Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.CBD.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.XGB_CBD.fem) {
  keep <- rep(TRUE, nrow(graph.XGB_CBD.fem))
  for (i in 1:nrow(graph.XGB_CBD.fem)) {
    for (j in 1:nrow(graph.XGB_CBD.fem)) {
      if (i != j) {
        if (graph.XGB_CBD.fem$rmse.train[j] <= graph.XGB_CBD.fem$rmse.train[i] &&
            graph.XGB_CBD.fem$rmse.test[j]  <= graph.XGB_CBD.fem$rmse.test[i] &&
            (graph.XGB_CBD.fem$rmse.train[j] < graph.XGB_CBD.fem$rmse.train[i] ||
             graph.XGB_CBD.fem$rmse.test[j]  < graph.XGB_CBD.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_CBD.fem$pareto <- pareto_front(graph.XGB_CBD.fem)

# Plot
Plot.EFF.XGB.CBD.fem <- ggplot(graph.XGB_CBD.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_CBD.fem[graph.XGB_CBD.fem$pareto, ][order(graph.XGB_CBD.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_CBD.fem[graph.XGB_CBD.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-CBD Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.XGB_CBD.male) {
  keep <- rep(TRUE, nrow(graph.XGB_CBD.male))
  for (i in 1:nrow(graph.XGB_CBD.male)) {
    for (j in 1:nrow(graph.XGB_CBD.male)) {
      if (i != j) {
        if (graph.XGB_CBD.male$rmse.train[j] <= graph.XGB_CBD.male$rmse.train[i] &&
            graph.XGB_CBD.male$rmse.test[j]  <= graph.XGB_CBD.male$rmse.test[i] &&
            (graph.XGB_CBD.male$rmse.train[j] < graph.XGB_CBD.male$rmse.train[i] ||
             graph.XGB_CBD.male$rmse.test[j]  < graph.XGB_CBD.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_CBD.male$pareto <- pareto_front(graph.XGB_CBD.male)

# Plot
Plot.EFF.XGB.CBD.male <- ggplot(graph.XGB_CBD.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_CBD.male[graph.XGB_CBD.male$pareto, ][order(graph.XGB_CBD.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_CBD.male[graph.XGB_CBD.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-CBD Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.XGB.CBD.comb <- Plot.EFF.XGB.CBD.fem + Plot.EFF.XGB.CBD.male 
print(Plot.EFF.XGB.CBD.comb)
