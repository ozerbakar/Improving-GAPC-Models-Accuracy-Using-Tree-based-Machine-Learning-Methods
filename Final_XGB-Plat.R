
Deaths.test.XGB.Plat <- Deaths.test
Deaths.test.XGB.Plat$psi.Plat <- 0

# XGB algorithm does not work with factor variable
data_list <- list(Deaths.Plat, Deaths.test.XGB.Plat)
data_list_encoded <- lapply(data_list, function(dummy) {
  createDummyFeatures(dummy, target = "psi.Plat")
})
Deaths.XGB.Plat <- data_list_encoded[[1]]
Deaths.test.XGB.Plat <- data_list_encoded[[2]]

# Creating learner
xgb <- makeLearner("regr.xgboost", predict.type = "response")

# Creating training and testing tasks
trainTask.XGB.Plat <- makeRegrTask(data = Deaths.XGB.Plat, target = "psi.Plat")
testTask.XGB.Plat <- makeRegrTask(data = Deaths.test.XGB.Plat, target = "psi.Plat")

# Creating parameters grid
xgb_params <- expand.grid(
  nrounds = c(50, 100, 150),
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_XGB.Plat.train <- Deaths.Plat
output_XGB.Plat.test <- Deaths.test.XGB.Plat

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
    task = trainTask.XGB.Plat,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.xgb <- train(pars.xgb, trainTask.XGB.Plat)
  
  # Prediction of the model
  preds.test.xgb <- predict(model.xgb, task = testTask.XGB.Plat)
  preds.train.xgb <- predict(model.xgb, task = trainTask.XGB.Plat)
  
  # Saving predictions
  output_XGB.Plat.test[[paste0("hp ",i)]] <- preds.test.xgb$data$response
  output_XGB.Plat.train[[paste0("hp ",i)]] <- preds.train.xgb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.Plat <- as.vector(as.matrix(for.m.Plat.fem))
Rate.test.male.Plat <- as.vector(as.matrix(for.m.Plat.male))

test.psi.fem.XGB.Plat <- output_XGB.Plat.test %>% filter(Gender.f == 1 & Gender.m == 0)
test.psi.fem.XGB.Plat <- test.psi.fem.XGB.Plat[-1:-6]
test.psi.male.XGB.Plat <- output_XGB.Plat.test %>% filter(Gender.f == 0 & Gender.m == 1)
test.psi.male.XGB.Plat <- test.psi.male.XGB.Plat[-1:-6]

test.imp.rates.fem.XGB.Plat <- as.data.frame(as.matrix(test.psi.fem.XGB.Plat)*Rate.test.fem.Plat)
test.imp.rates.fem.XGB.Plat$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.XGB.Plat <- as.data.frame(as.matrix(test.psi.male.XGB.Plat)*Rate.test.male.Plat)
test.imp.rates.male.XGB.Plat$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.XGB.Plat.fem <- c()
test.rmse_results.XGB.Plat.male <- c()

for (i in colnames(test.imp.rates.fem.XGB.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.Plat.fem <- RMSE(test.imp.rates.fem.XGB.Plat[[i]], test.imp.rates.fem.XGB.Plat$obs.test)
    test.rmse_results.XGB.Plat.fem[i] <- test.rmse_val.XGB.Plat.fem
  }
}

for (i in colnames(test.imp.rates.male.XGB.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.Plat.male <- RMSE(test.imp.rates.male.XGB.Plat[[i]], test.imp.rates.male.XGB.Plat$obs.test)
    test.rmse_results.XGB.Plat.male[i] <- test.rmse_val.XGB.Plat.male
  }
}

test.rmse.XGB.Plat.fem <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.Plat.fem),
  rmse.test = as.numeric(test.rmse_results.XGB.Plat.fem)
)
test.rmse.XGB.Plat.male <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.Plat.male),
  rmse.test = as.numeric(test.rmse_results.XGB.Plat.male)
)

test.rmse.lower.XGB.Plat.fem <- test.rmse.XGB.Plat.fem %>% filter(rmse.test < rmse.for.Plat.fem)
test.rmse.lower.XGB.Plat.male <- test.rmse.XGB.Plat.male %>% filter(rmse.test < rmse.for.Plat.male)

Rate.train.fem.Plat <- as.vector(as.matrix(m.Plat.fem))
Rate.train.male.Plat <- as.vector(as.matrix(m.Plat.male))

train.psi.fem.XGB.Plat <- output_XGB.Plat.train %>% filter(Gender == "f")
train.psi.fem.XGB.Plat <- train.psi.fem.XGB.Plat[-1:-5]
train.psi.male.XGB.Plat <- output_XGB.Plat.train %>% filter(Gender == "m")
train.psi.male.XGB.Plat <- train.psi.male.XGB.Plat[-1:-5]

train.imp.rates.fem.XGB.Plat <- as.data.frame(as.matrix(train.psi.fem.XGB.Plat)*Rate.train.fem.Plat)
train.imp.rates.fem.XGB.Plat$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.XGB.Plat <- as.data.frame(as.matrix(train.psi.male.XGB.Plat)*Rate.train.male.Plat)
train.imp.rates.male.XGB.Plat$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.XGB.Plat.fem <- c()
train.rmse_results.XGB.Plat.male <- c()

for (i in colnames(train.imp.rates.fem.XGB.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.Plat.fem <- RMSE(train.imp.rates.fem.XGB.Plat[[i]], train.imp.rates.fem.XGB.Plat$obs.train)
    train.rmse_results.XGB.Plat.fem[i] <- train.rmse_val.XGB.Plat.fem
  }
}
for (i in colnames(train.imp.rates.male.XGB.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.Plat.male <- RMSE(train.imp.rates.male.XGB.Plat[[i]], train.imp.rates.male.XGB.Plat$obs.train)
    train.rmse_results.XGB.Plat.male[i] <- train.rmse_val.XGB.Plat.male
  }
}
train.rmse.XGB.Plat.fem <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.Plat.fem),
  rmse.train = as.numeric(train.rmse_results.XGB.Plat.fem))
train.rmse.XGB.Plat.male <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.Plat.male),
  rmse.train = as.numeric(train.rmse_results.XGB.Plat.male))

train.rmse.lower.XGB.Plat.fem <- train.rmse.XGB.Plat.fem %>% filter(rmse.train < rmse.fit.Plat.fem)
tt.rmse.XGB.Plat.fem <- inner_join(test.rmse.lower.XGB.Plat.fem, train.rmse.lower.XGB.Plat.fem, by = "Hyper.p")
train.rmse.lower.XGB.Plat.male <- train.rmse.XGB.Plat.male %>% filter(rmse.train < rmse.fit.Plat.male)
tt.rmse.XGB.Plat.male <- inner_join(test.rmse.lower.XGB.Plat.male, train.rmse.lower.XGB.Plat.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.XGB.Plat.fem$rmse.test <- round(tt.rmse.XGB.Plat.fem$rmse.test, digits = 8)
tt.rmse.XGB.Plat.fem$rmse.train <- round(tt.rmse.XGB.Plat.fem$rmse.train, digits = 8)
tt.rmse.XGB.Plat.male$rmse.test <- round(tt.rmse.XGB.Plat.male$rmse.test, digits = 8)
tt.rmse.XGB.Plat.male$rmse.train <- round(tt.rmse.XGB.Plat.male$rmse.train, digits = 8)

graph.XGB_Plat.fem <- tt.rmse.XGB.Plat.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.XGB_Plat.male <- tt.rmse.XGB.Plat.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.XGB_Plat.fem <- graph.XGB_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.XGB_Plat.male <- graph.XGB_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.XGB_Plat.fem <- graph.XGB_Plat.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.XGB_Plat.male <- graph.XGB_Plat.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.XGB_Plat.fem <- graph.XGB_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.XGB_Plat.male <- graph.XGB_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.XGB_Plat.fem$Hyper.p <- factor(graph.XGB_Plat.fem$Hyper.p,
                                    levels = graph.XGB_Plat.fem$Hyper.p[order(as.numeric(str_extract(graph.XGB_Plat.fem$Hyper.p, "[0-9]+")))])
graph.XGB_Plat.male$Hyper.p <- factor(graph.XGB_Plat.male$Hyper.p,
                                     levels = graph.XGB_Plat.male$Hyper.p[order(as.numeric(str_extract(graph.XGB_Plat.male$Hyper.p, "[0-9]+")))])

plot.XGB.Plat.fem.test <- ggplot(graph.XGB_Plat.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.Plat.fem.train <- ggplot(graph.XGB_Plat.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.Plat.fem.comb <- plot.XGB.Plat.fem.train + plot.XGB.Plat.fem.test + 
  plot_annotation(title = "XGB-Plat Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.Plat.fem.comb)

plot.XGB.Plat.male.test <- ggplot(graph.XGB_Plat.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.Plat.male.train <- ggplot(graph.XGB_Plat.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.Plat.male.comb <- plot.XGB.Plat.male.train + plot.XGB.Plat.male.test + 
  plot_annotation(title = "XGB-Plat Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.Plat.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.XGB_Plat.fem) {
  keep <- rep(TRUE, nrow(graph.XGB_Plat.fem))
  for (i in 1:nrow(graph.XGB_Plat.fem)) {
    for (j in 1:nrow(graph.XGB_Plat.fem)) {
      if (i != j) {
        if (graph.XGB_Plat.fem$rmse.train[j] <= graph.XGB_Plat.fem$rmse.train[i] &&
            graph.XGB_Plat.fem$rmse.test[j]  <= graph.XGB_Plat.fem$rmse.test[i] &&
            (graph.XGB_Plat.fem$rmse.train[j] < graph.XGB_Plat.fem$rmse.train[i] ||
             graph.XGB_Plat.fem$rmse.test[j]  < graph.XGB_Plat.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_Plat.fem$pareto <- pareto_front(graph.XGB_Plat.fem)

# Plot
Plot.EFF.XGB.Plat.fem <- ggplot(graph.XGB_Plat.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_Plat.fem[graph.XGB_Plat.fem$pareto, ][order(graph.XGB_Plat.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_Plat.fem[graph.XGB_Plat.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-Plat Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.XGB_Plat.male) {
  keep <- rep(TRUE, nrow(graph.XGB_Plat.male))
  for (i in 1:nrow(graph.XGB_Plat.male)) {
    for (j in 1:nrow(graph.XGB_Plat.male)) {
      if (i != j) {
        if (graph.XGB_Plat.male$rmse.train[j] <= graph.XGB_Plat.male$rmse.train[i] &&
            graph.XGB_Plat.male$rmse.test[j]  <= graph.XGB_Plat.male$rmse.test[i] &&
            (graph.XGB_Plat.male$rmse.train[j] < graph.XGB_Plat.male$rmse.train[i] ||
             graph.XGB_Plat.male$rmse.test[j]  < graph.XGB_Plat.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_Plat.male$pareto <- pareto_front(graph.XGB_Plat.male)

# Plot
Plot.EFF.XGB.Plat.male <- ggplot(graph.XGB_Plat.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_Plat.male[graph.XGB_Plat.male$pareto, ][order(graph.XGB_Plat.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_Plat.male[graph.XGB_Plat.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-Plat Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.XGB.Plat.comb <- Plot.EFF.XGB.Plat.fem + Plot.EFF.XGB.Plat.male 
print(Plot.EFF.XGB.Plat.comb)
