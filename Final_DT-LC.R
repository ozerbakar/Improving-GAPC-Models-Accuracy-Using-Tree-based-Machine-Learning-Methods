
Deaths.test.DT.LC <- Deaths.test
Deaths.test.DT.LC$psi.LC <- 0

# Creating learner
dt <- makeLearner("regr.rpart", predict.type = "response")

# Creating training and testing tasks
trainTask.DT.LC <- makeRegrTask(data = Deaths.LC, target = "psi.LC")
testTask.DT.LC <- makeRegrTask(data = Deaths.test.DT.LC, target = "psi.LC")

# Creating parameters grid
dt_params <- expand.grid(
  cp = c(0.001),
  minsplit = c(1: 10),
  maxdepth = c(1:30),
  minbucket = c(1:20)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_DT.LC.train <- Deaths.LC
output_DT.LC.test <- Deaths.test.DT.LC

# Calibration of psi over all hyper-parameter combinations
for (i in 1:nrow(dt_params)) {
  hp.dt <- dt_params[i, ]
  
  # Selecting hyper-parameter set
  pars.dt <- setHyperPars(
    learner = dt,
    par.vals = list(
      cp = hp.dt$cp,
      minsplit = hp.dt$minsplit,
      minbucket = hp.dt$minbucket,
      maxdepth = hp.dt$maxdepth
    )
  )
  
  # Cross-validation
  resample_result <- resample(
    learner = pars.dt,
    task = trainTask.DT.LC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.dt <- train(pars.dt, trainTask.DT.LC)
  
  # Prediction of the model
  preds.test.dt <- predict(model.dt, task = testTask.DT.LC)
  preds.train.dt <- predict(model.dt, task = trainTask.DT.LC)
  
  # Saving predictions
  output_DT.LC.test[[paste0("hp ",i)]] <- preds.test.dt$data$response
  output_DT.LC.train[[paste0("hp ",i)]] <- preds.train.dt$data$response
}

# Calculating improved mortality rates and errors of each series
Rate.test.fem.LC <- as.vector(as.matrix(for.m.LC.fem))
Rate.test.male.LC <- as.vector(as.matrix(for.m.LC.male))

test.psi.fem.DT.LC <- output_DT.LC.test %>% filter(Gender == "f")
test.psi.fem.DT.LC <- test.psi.fem.DT.LC[-1:-5]
test.psi.male.DT.LC <- output_DT.LC.test %>% filter(Gender == "m")
test.psi.male.DT.LC <- test.psi.male.DT.LC[-1:-5]

test.imp.rates.fem.DT.LC <- as.data.frame(as.matrix(test.psi.fem.DT.LC)*Rate.test.fem.LC)
test.imp.rates.fem.DT.LC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.DT.LC <- as.data.frame(as.matrix(test.psi.male.DT.LC)*Rate.test.male.LC)
test.imp.rates.male.DT.LC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.DT.LC.fem <- c()
test.rmse_results.DT.LC.male <- c()

for (i in colnames(test.imp.rates.fem.DT.LC)) {
  if (i != "obs.test") {
    test.rmse_val.DT.LC.fem <- RMSE(test.imp.rates.fem.DT.LC[[i]], test.imp.rates.fem.DT.LC$obs.test)
    test.rmse_results.DT.LC.fem[i] <- test.rmse_val.DT.LC.fem
  }
}

for (i in colnames(test.imp.rates.male.DT.LC)) {
  if (i != "obs.test") {
    test.rmse_val.DT.LC.male <- RMSE(test.imp.rates.male.DT.LC[[i]], test.imp.rates.male.DT.LC$obs.test)
    test.rmse_results.DT.LC.male[i] <- test.rmse_val.DT.LC.male
  }
}

test.rmse.DT.LC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.DT.LC.fem),
  rmse.test = as.numeric(test.rmse_results.DT.LC.fem)
)
test.rmse.DT.LC.male <- data.frame(
  Hyper.p = names(test.rmse_results.DT.LC.male),
  rmse.test = as.numeric(test.rmse_results.DT.LC.male)
)

test.rmse.lower.DT.LC.fem <- test.rmse.DT.LC.fem %>% filter(rmse.test < rmse.for.LC.fem)
test.rmse.lower.DT.LC.male <- test.rmse.DT.LC.male %>% filter(rmse.test < rmse.for.LC.male)

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.train.fem.LC <- as.vector(as.matrix(m.LC.fem))
Rate.train.male.LC <- as.vector(as.matrix(m.LC.male))

train.psi.fem.DT.LC <- output_DT.LC.train %>% filter(Gender == "f")
train.psi.fem.DT.LC <- train.psi.fem.DT.LC[-1:-5]
train.psi.male.DT.LC <- output_DT.LC.train %>% filter(Gender == "m")
train.psi.male.DT.LC <- train.psi.male.DT.LC[-1:-5]

train.imp.rates.fem.DT.LC <- as.data.frame(as.matrix(train.psi.fem.DT.LC)*Rate.train.fem.LC)
train.imp.rates.fem.DT.LC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.DT.LC <- as.data.frame(as.matrix(train.psi.male.DT.LC)*Rate.train.male.LC)
train.imp.rates.male.DT.LC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.DT.LC.fem <- c()
train.rmse_results.DT.LC.male <- c()

for (i in colnames(train.imp.rates.fem.DT.LC)) {
  if (i != "obs.train") {
    train.rmse_val.DT.LC.fem <- RMSE(train.imp.rates.fem.DT.LC[[i]], train.imp.rates.fem.DT.LC$obs.train)
    train.rmse_results.DT.LC.fem[i] <- train.rmse_val.DT.LC.fem
  }
}
for (i in colnames(train.imp.rates.male.DT.LC)) {
  if (i != "obs.train") {
    train.rmse_val.DT.LC.male <- RMSE(train.imp.rates.male.DT.LC[[i]], train.imp.rates.male.DT.LC$obs.train)
    train.rmse_results.DT.LC.male[i] <- train.rmse_val.DT.LC.male
  }
}
train.rmse.DT.LC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.DT.LC.fem),
  rmse.train = as.numeric(train.rmse_results.DT.LC.fem))
train.rmse.DT.LC.male <- data.frame(
  Hyper.p = names(train.rmse_results.DT.LC.male),
  rmse.train = as.numeric(train.rmse_results.DT.LC.male))

train.rmse.lower.DT.LC.fem <- train.rmse.DT.LC.fem %>% filter(rmse.train < rmse.fit.LC.fem)
tt.rmse.DT.LC.fem <- inner_join(test.rmse.lower.DT.LC.fem, train.rmse.lower.DT.LC.fem, by = "Hyper.p")
train.rmse.lower.DT.LC.male <- train.rmse.DT.LC.male %>% filter(rmse.train < rmse.fit.LC.male)
tt.rmse.DT.LC.male <- inner_join(test.rmse.lower.DT.LC.male, train.rmse.lower.DT.LC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.DT.LC.fem$rmse.test <- round(tt.rmse.DT.LC.fem$rmse.test, digits = 8)
tt.rmse.DT.LC.fem$rmse.train <- round(tt.rmse.DT.LC.fem$rmse.train, digits = 8)
tt.rmse.DT.LC.male$rmse.test <- round(tt.rmse.DT.LC.male$rmse.test, digits = 8)
tt.rmse.DT.LC.male$rmse.train <- round(tt.rmse.DT.LC.male$rmse.train, digits = 8)

graph.DT_LC.fem <- tt.rmse.DT.LC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.DT_LC.male <- tt.rmse.DT.LC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

# Calculating minimum and corresponding errors for both periods
min.test.DT_LC.fem <- graph.DT_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.DT_LC.male <- graph.DT_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.DT_LC.fem <- graph.DT_LC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.DT_LC.male <- graph.DT_LC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.DT_LC.fem <- graph.DT_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.DT_LC.male <- graph.DT_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.DT_LC.fem$Hyper.p <- factor(graph.DT_LC.fem$Hyper.p,
                                  levels = graph.DT_LC.fem$Hyper.p[order(as.numeric(str_extract(graph.DT_LC.fem$Hyper.p, "[0-9]+")))])
graph.DT_LC.male$Hyper.p <- factor(graph.DT_LC.male$Hyper.p,
                                  levels = graph.DT_LC.male$Hyper.p[order(as.numeric(str_extract(graph.DT_LC.male$Hyper.p, "[0-9]+")))])
# Plotting
plot.DT.LC.fem.test <- ggplot(graph.DT_LC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2),
        plot.title = element_text(hjust = 0.5))

plot.DT.LC.fem.train <- ggplot(graph.DT_LC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 2),
        plot.title = element_text(hjust = 0.5))

Plot.DT.LC.fem.comb <- plot.DT.LC.fem.train + plot.DT.LC.fem.test + 
  plot_annotation(title = "DT-LC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
print(Plot.DT.LC.fem.comb)

plot.DT.LC.male.test <- ggplot(graph.DT_LC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.LC.male.train <- ggplot(graph.DT_LC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.LC.male.comb <- plot.DT.LC.male.train + plot.DT.LC.male.test + 
  plot_annotation(title = "DT-LC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
print(Plot.DT.LC.male.comb)

## Graph for Efficient Frontier

# Pareto optimal function
pareto_front <- function(graph.DT_LC.fem) {
  keep <- rep(TRUE, nrow(graph.DT_LC.fem))
  for (i in 1:nrow(graph.DT_LC.fem)) {
    for (j in 1:nrow(graph.DT_LC.fem)) {
      if (i != j) {
        if (graph.DT_LC.fem$rmse.train[j] <= graph.DT_LC.fem$rmse.train[i] &&
            graph.DT_LC.fem$rmse.test[j]  <= graph.DT_LC.fem$rmse.test[i] &&
            (graph.DT_LC.fem$rmse.train[j] < graph.DT_LC.fem$rmse.train[i] ||
             graph.DT_LC.fem$rmse.test[j]  < graph.DT_LC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_LC.fem$pareto <- pareto_front(graph.DT_LC.fem)

Plot.EFF.DT.LC.fem <- ggplot(graph.DT_LC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_LC.fem[graph.DT_LC.fem$pareto, ][order(graph.DT_LC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_LC.fem[graph.DT_LC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-LC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

pareto_front <- function(graph.DT_LC.male) {
  keep <- rep(TRUE, nrow(graph.DT_LC.male))
  for (i in 1:nrow(graph.DT_LC.male)) {
    for (j in 1:nrow(graph.DT_LC.male)) {
      if (i != j) {
        if (graph.DT_LC.male$rmse.train[j] <= graph.DT_LC.male$rmse.train[i] &&
            graph.DT_LC.male$rmse.test[j]  <= graph.DT_LC.male$rmse.test[i] &&
            (graph.DT_LC.male$rmse.train[j] < graph.DT_LC.male$rmse.train[i] ||
             graph.DT_LC.male$rmse.test[j]  < graph.DT_LC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_LC.male$pareto <- pareto_front(graph.DT_LC.male)

Plot.EFF.DT.LC.male <- ggplot(graph.DT_LC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_LC.male[graph.DT_LC.male$pareto, ][order(graph.DT_LC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_LC.male[graph.DT_LC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-LC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.DT.LC.comb <- Plot.EFF.DT.LC.fem + Plot.EFF.DT.LC.male 
print(Plot.EFF.DT.LC.comb)
