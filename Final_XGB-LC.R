
Deaths.test.XGB.LC <- Deaths.test
Deaths.test.XGB.LC$psi.LC <- 0

# XGB algorithm does not work with factor variable
data_list <- list(Deaths.LC, Deaths.test.XGB.LC)
data_list_encoded <- lapply(data_list, function(dummy) {
  createDummyFeatures(dummy, target = "psi.LC")
})
Deaths.XGB.LC <- data_list_encoded[[1]]
Deaths.test.XGB.LC <- data_list_encoded[[2]]

# Creating learner
xgb <- makeLearner("regr.xgboost", predict.type = "response")

# Creating training and testing tasks
trainTask.XGB.LC <- makeRegrTask(data = Deaths.XGB.LC, target = "psi.LC")
testTask.XGB.LC <- makeRegrTask(data = Deaths.test.XGB.LC, target = "psi.LC")

# Creating parameters grid
xgb_params <- expand.grid(
  nrounds = c(50, 100, 150),
  eta = c(0.01, 0.05, 0.1),
  max_depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_XGB.LC.train <- Deaths.LC
output_XGB.LC.test <- Deaths.test.XGB.LC

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
    task = trainTask.XGB.LC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.xgb <- train(pars.xgb, trainTask.XGB.LC)
  
  # Prediction of the model
  preds.test.xgb <- predict(model.xgb, task = testTask.XGB.LC)
  preds.train.xgb <- predict(model.xgb, task = trainTask.XGB.LC)
  
  # Saving predictions
  output_XGB.LC.test[[paste0("hp ",i)]] <- preds.test.xgb$data$response
  output_XGB.LC.train[[paste0("hp ",i)]] <- preds.train.xgb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.LC <- as.vector(as.matrix(for.m.LC.fem))
Rate.test.male.LC <- as.vector(as.matrix(for.m.LC.male))

test.psi.fem.XGB.LC <- output_XGB.LC.test %>% filter(Gender.f == 1 & Gender.m == 0)
test.psi.fem.XGB.LC <- test.psi.fem.XGB.LC[-1:-6]
test.psi.male.XGB.LC <- output_XGB.LC.test %>% filter(Gender.f == 0 & Gender.m == 1)
test.psi.male.XGB.LC <- test.psi.male.XGB.LC[-1:-6]

test.imp.rates.fem.XGB.LC <- as.data.frame(as.matrix(test.psi.fem.XGB.LC)*Rate.test.fem.LC)
test.imp.rates.fem.XGB.LC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.XGB.LC <- as.data.frame(as.matrix(test.psi.male.XGB.LC)*Rate.test.male.LC)
test.imp.rates.male.XGB.LC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.XGB.LC.fem <- c()
test.rmse_results.XGB.LC.male <- c()

for (i in colnames(test.imp.rates.fem.XGB.LC)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.LC.fem <- RMSE(test.imp.rates.fem.XGB.LC[[i]], test.imp.rates.fem.XGB.LC$obs.test)
    test.rmse_results.XGB.LC.fem[i] <- test.rmse_val.XGB.LC.fem
  }
}

for (i in colnames(test.imp.rates.male.XGB.LC)) {
  if (i != "obs.test") {
    test.rmse_val.XGB.LC.male <- RMSE(test.imp.rates.male.XGB.LC[[i]], test.imp.rates.male.XGB.LC$obs.test)
    test.rmse_results.XGB.LC.male[i] <- test.rmse_val.XGB.LC.male
  }
}

test.rmse.XGB.LC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.LC.fem),
  rmse.test = as.numeric(test.rmse_results.XGB.LC.fem)
)
test.rmse.XGB.LC.male <- data.frame(
  Hyper.p = names(test.rmse_results.XGB.LC.male),
  rmse.test = as.numeric(test.rmse_results.XGB.LC.male)
)

test.rmse.lower.XGB.LC.fem <- test.rmse.XGB.LC.fem %>% filter(rmse.test < rmse.for.LC.fem)
test.rmse.lower.XGB.LC.male <- test.rmse.XGB.LC.male %>% filter(rmse.test < rmse.for.LC.male)

Rate.train.fem.LC <- as.vector(as.matrix(m.LC.fem))
Rate.train.male.LC <- as.vector(as.matrix(m.LC.male))

train.psi.fem.XGB.LC <- output_XGB.LC.train %>% filter(Gender == "f")
train.psi.fem.XGB.LC <- train.psi.fem.XGB.LC[-1:-5]
train.psi.male.XGB.LC <- output_XGB.LC.train %>% filter(Gender == "m")
train.psi.male.XGB.LC <- train.psi.male.XGB.LC[-1:-5]

train.imp.rates.fem.XGB.LC <- as.data.frame(as.matrix(train.psi.fem.XGB.LC)*Rate.train.fem.LC)
train.imp.rates.fem.XGB.LC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.XGB.LC <- as.data.frame(as.matrix(train.psi.male.XGB.LC)*Rate.train.male.LC)
train.imp.rates.male.XGB.LC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.XGB.LC.fem <- c()
train.rmse_results.XGB.LC.male <- c()

for (i in colnames(train.imp.rates.fem.XGB.LC)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.LC.fem <- RMSE(train.imp.rates.fem.XGB.LC[[i]], train.imp.rates.fem.XGB.LC$obs.train)
    train.rmse_results.XGB.LC.fem[i] <- train.rmse_val.XGB.LC.fem
  }
}
for (i in colnames(train.imp.rates.male.XGB.LC)) {
  if (i != "obs.train") {
    train.rmse_val.XGB.LC.male <- RMSE(train.imp.rates.male.XGB.LC[[i]], train.imp.rates.male.XGB.LC$obs.train)
    train.rmse_results.XGB.LC.male[i] <- train.rmse_val.XGB.LC.male
  }
}
train.rmse.XGB.LC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.LC.fem),
  rmse.train = as.numeric(train.rmse_results.XGB.LC.fem))
train.rmse.XGB.LC.male <- data.frame(
  Hyper.p = names(train.rmse_results.XGB.LC.male),
  rmse.train = as.numeric(train.rmse_results.XGB.LC.male))

train.rmse.lower.XGB.LC.fem <- train.rmse.XGB.LC.fem %>% filter(rmse.train < rmse.fit.LC.fem)
tt.rmse.XGB.LC.fem <- inner_join(test.rmse.lower.XGB.LC.fem, train.rmse.lower.XGB.LC.fem, by = "Hyper.p")
train.rmse.lower.XGB.LC.male <- train.rmse.XGB.LC.male %>% filter(rmse.train < rmse.fit.LC.male)
tt.rmse.XGB.LC.male <- inner_join(test.rmse.lower.XGB.LC.male, train.rmse.lower.XGB.LC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.XGB.LC.fem$rmse.test <- round(tt.rmse.XGB.LC.fem$rmse.test, digits = 8)
tt.rmse.XGB.LC.fem$rmse.train <- round(tt.rmse.XGB.LC.fem$rmse.train, digits = 8)
tt.rmse.XGB.LC.male$rmse.test <- round(tt.rmse.XGB.LC.male$rmse.test, digits = 8)
tt.rmse.XGB.LC.male$rmse.train <- round(tt.rmse.XGB.LC.male$rmse.train, digits = 8)

graph.XGB_LC.fem <- tt.rmse.XGB.LC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.XGB_LC.male <- tt.rmse.XGB.LC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.XGB_LC.fem <- graph.XGB_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.XGB_LC.male <- graph.XGB_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.XGB_LC.fem <- graph.XGB_LC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.XGB_LC.male <- graph.XGB_LC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.XGB_LC.fem <- graph.XGB_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.XGB_LC.male <- graph.XGB_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.XGB_LC.fem$Hyper.p <- factor(graph.XGB_LC.fem$Hyper.p,
                                  levels = graph.XGB_LC.fem$Hyper.p[order(as.numeric(str_extract(graph.XGB_LC.fem$Hyper.p, "[0-9]+")))])
graph.XGB_LC.male$Hyper.p <- factor(graph.XGB_LC.male$Hyper.p,
                                    levels = graph.XGB_LC.male$Hyper.p[order(as.numeric(str_extract(graph.XGB_LC.male$Hyper.p, "[0-9]+")))])

plot.XGB.LC.fem.test <- ggplot(graph.XGB_LC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.LC.fem.train <- ggplot(graph.XGB_LC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.LC.fem.comb <- plot.XGB.LC.fem.train + plot.XGB.LC.fem.test + 
  plot_annotation(title = "XGB-LC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.LC.fem.comb)

plot.XGB.LC.male.test <- ggplot(graph.XGB_LC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.XGB.LC.male.train <- ggplot(graph.XGB_LC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.XGB.LC.male.comb <- plot.XGB.LC.male.train + plot.XGB.LC.male.test + 
  plot_annotation(title = "XGB-LC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.XGB.LC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.XGB_LC.fem) {
  keep <- rep(TRUE, nrow(graph.XGB_LC.fem))
  for (i in 1:nrow(graph.XGB_LC.fem)) {
    for (j in 1:nrow(graph.XGB_LC.fem)) {
      if (i != j) {
        if (graph.XGB_LC.fem$rmse.train[j] <= graph.XGB_LC.fem$rmse.train[i] &&
            graph.XGB_LC.fem$rmse.test[j]  <= graph.XGB_LC.fem$rmse.test[i] &&
            (graph.XGB_LC.fem$rmse.train[j] < graph.XGB_LC.fem$rmse.train[i] ||
             graph.XGB_LC.fem$rmse.test[j]  < graph.XGB_LC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_LC.fem$pareto <- pareto_front(graph.XGB_LC.fem)

# Plot
Plot.EFF.XGB.LC.fem <- ggplot(graph.XGB_LC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_LC.fem[graph.XGB_LC.fem$pareto, ][order(graph.XGB_LC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_LC.fem[graph.XGB_LC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-LC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.XGB_LC.male) {
  keep <- rep(TRUE, nrow(graph.XGB_LC.male))
  for (i in 1:nrow(graph.XGB_LC.male)) {
    for (j in 1:nrow(graph.XGB_LC.male)) {
      if (i != j) {
        if (graph.XGB_LC.male$rmse.train[j] <= graph.XGB_LC.male$rmse.train[i] &&
            graph.XGB_LC.male$rmse.test[j]  <= graph.XGB_LC.male$rmse.test[i] &&
            (graph.XGB_LC.male$rmse.train[j] < graph.XGB_LC.male$rmse.train[i] ||
             graph.XGB_LC.male$rmse.test[j]  < graph.XGB_LC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.XGB_LC.male$pareto <- pareto_front(graph.XGB_LC.male)

# Plot
Plot.EFF.XGB.LC.male <- ggplot(graph.XGB_LC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.XGB_LC.male[graph.XGB_LC.male$pareto, ][order(graph.XGB_LC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.XGB_LC.male[graph.XGB_LC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "XGB-LC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.XGB.LC.comb <- Plot.EFF.XGB.LC.fem + Plot.EFF.XGB.LC.male 
print(Plot.EFF.XGB.LC.comb)
