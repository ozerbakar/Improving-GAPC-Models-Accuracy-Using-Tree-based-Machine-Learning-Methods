
Deaths.test.CBD <- Deaths.test
Deaths.test.CBD$psi.CBD <- 0

# Creating learner
dt <- makeLearner("regr.rpart", predict.type = "response")

# Creating training and testing tasks
trainTask.DT.CBD <- makeRegrTask(data = Deaths.CBD, target = "psi.CBD")
testTask.DT.CBD <- makeRegrTask(data = Deaths.test.CBD, target = "psi.CBD")

# Creating parameters grid
dt_params <- expand.grid(
  cp = c(0.001),
  minsplit = c(1: 10),
  maxdepth = c(1:10),
  minbucket = c(1:10)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 2L)

# Create outputs for saving estimations
output_CBD.train <- Deaths.CBD
output_CBD.test <- Deaths.test.CBD

# Calibration of psi over all hyper-parameter combinations
for (i in 1:nrow(dt_params)) {
  hp <- dt_params[i, ]
  
  # Selecting hyper-parameter set
  pars <- setHyperPars(
    learner = dt,
    par.vals = list(
      cp = hp$cp,
      minsplit = hp$minsplit,
      minbucket = hp$minbucket,
      maxdepth = hp$maxdepth
    )
  )
  
  # Cross-validation
  resample_result <- resample(
    learner = pars,
    task = trainTask.DT.CBD,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model <- train(pars, trainTask.DT.CBD)
  
  # Prediction of the model
  preds.test <- predict(model, task = testTask.DT.CBD)
  preds.train <- predict(model, task = trainTask.DT.CBD)
  
  # Saving predictions
  output_CBD.test[[paste0("hp ",i)]] <- preds.test$data$response
  output_CBD.train[[paste0("hp ",i)]] <- preds.train$data$response
}

# Calculating improved mortality rates and errors of each series
Rate.test.fem.CBD <- as.vector(as.matrix(for.m.CBD.fem))
Rate.test.male.CBD <- as.vector(as.matrix(for.m.CBD.male))

test.psi.fem.CBD <- output_CBD.test %>% filter(Gender == "f")
test.psi.fem.CBD <- test.psi.fem.CBD[-1:-5]
test.psi.male.CBD <- output_CBD.test %>% filter(Gender == "m")
test.psi.male.CBD <- test.psi.male.CBD[-1:-5]

test.imp.rates.fem.CBD <- as.data.frame(as.matrix(test.psi.fem.CBD)*Rate.test.fem.CBD)
test.imp.rates.fem.CBD$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.CBD <- as.data.frame(as.matrix(test.psi.male.CBD)*Rate.test.male.CBD)
test.imp.rates.male.CBD$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.CBD.fem <- c()
test.rmse_results.CBD.male <- c()

for (i in colnames(test.imp.rates.fem.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.CBD.fem <- RMSE(test.imp.rates.fem.CBD[[i]], test.imp.rates.fem.CBD$obs.test)
    test.rmse_results.CBD.fem[i] <- test.rmse_val.CBD.fem
  }
}

for (i in colnames(test.imp.rates.male.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.CBD.male <- RMSE(test.imp.rates.male.CBD[[i]], test.imp.rates.male.CBD$obs.test)
    test.rmse_results.CBD.male[i] <- test.rmse_val.CBD.male
  }
}

test.rmse.CBD.fem <- data.frame(
  Hyper.p = names(test.rmse_results.CBD.fem),
  rmse.test = as.numeric(test.rmse_results.CBD.fem)
)
test.rmse.CBD.male <- data.frame(
  Hyper.p = names(test.rmse_results.CBD.male),
  rmse.test = as.numeric(test.rmse_results.CBD.male)
)

test.rmse.lower.CBD.fem <- test.rmse.CBD.fem %>% filter(rmse.test < rmse.for.CBD.fem)
test.rmse.lower.CBD.male <- test.rmse.CBD.male %>% filter(rmse.test < rmse.for.CBD.male)

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.train.fem.CBD <- as.vector(as.matrix(m.CBD.fem))
Rate.train.male.CBD <- as.vector(as.matrix(m.CBD.male))

train.psi.fem.CBD <- output_CBD.train %>% filter(Gender == "f")
train.psi.fem.CBD <- train.psi.fem.CBD[-1:-5]
train.psi.male.CBD <- output_CBD.train %>% filter(Gender == "m")
train.psi.male.CBD <- train.psi.male.CBD[-1:-5]

train.imp.rates.fem.CBD <- as.data.frame(as.matrix(train.psi.fem.CBD)*Rate.train.fem.CBD)
train.imp.rates.fem.CBD$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.CBD <- as.data.frame(as.matrix(train.psi.male.CBD)*Rate.train.male.CBD)
train.imp.rates.male.CBD$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.CBD.fem <- c()
train.rmse_results.CBD.male <- c()

for (i in colnames(train.imp.rates.fem.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.CBD.fem <- RMSE(train.imp.rates.fem.CBD[[i]], train.imp.rates.fem.CBD$obs.train)
    train.rmse_results.CBD.fem[i] <- train.rmse_val.CBD.fem
  }
}
for (i in colnames(train.imp.rates.male.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.CBD.male <- RMSE(train.imp.rates.male.CBD[[i]], train.imp.rates.male.CBD$obs.train)
    train.rmse_results.CBD.male[i] <- train.rmse_val.CBD.male
  }
}
train.rmse.CBD.fem <- data.frame(
  Hyper.p = names(train.rmse_results.CBD.fem),
  rmse.train = as.numeric(train.rmse_results.CBD.fem))
train.rmse.CBD.male <- data.frame(
  Hyper.p = names(train.rmse_results.CBD.male),
  rmse.train = as.numeric(train.rmse_results.CBD.male))

train.rmse.lower.CBD.fem <- train.rmse.CBD.fem %>% filter(rmse.train < rmse.fit.CBD.fem)
tt.rmse.CBD.fem <- inner_join(test.rmse.lower.CBD.fem, train.rmse.lower.CBD.fem, by = "Hyper.p")
train.rmse.lower.CBD.male <- train.rmse.CBD.male %>% filter(rmse.train < rmse.fit.CBD.male)
tt.rmse.CBD.male <- inner_join(test.rmse.lower.CBD.male, train.rmse.lower.CBD.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.CBD.fem$rmse.test <- round(tt.rmse.CBD.fem$rmse.test, digits = 8)
tt.rmse.CBD.fem$rmse.train <- round(tt.rmse.CBD.fem$rmse.train, digits = 8)
tt.rmse.CBD.male$rmse.test <- round(tt.rmse.CBD.male$rmse.test, digits = 8)
tt.rmse.CBD.male$rmse.train <- round(tt.rmse.CBD.male$rmse.train, digits = 8)

graph.DT_CBD.fem <- tt.rmse.CBD.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.DT_CBD.male <- tt.rmse.CBD.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.DT_CBD.fem <- graph.DT_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.DT_CBD.male <- graph.DT_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.DT_CBD.fem <- graph.DT_CBD.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.DT_CBD.male <- graph.DT_CBD.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.DT_CBD.fem <- graph.DT_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.DT_CBD.male <- graph.DT_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.DT_CBD.fem$Hyper.p <- factor(graph.DT_CBD.fem$Hyper.p,
                                  levels = graph.DT_CBD.fem$Hyper.p[order(as.numeric(str_extract(graph.DT_CBD.fem$Hyper.p, "[0-9]+")))])
graph.DT_CBD.male$Hyper.p <- factor(graph.DT_CBD.male$Hyper.p,
                                   levels = graph.DT_CBD.male$Hyper.p[order(as.numeric(str_extract(graph.DT_CBD.male$Hyper.p, "[0-9]+")))])

plot.DT.CBD.fem.test <- ggplot(graph.DT_CBD.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.CBD.fem.train <- ggplot(graph.DT_CBD.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.CBD.fem.comb <- plot.DT.CBD.fem.train + plot.DT.CBD.fem.test + 
  plot_annotation(title = "DT-CBD Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
print(Plot.DT.CBD.fem.comb)

plot.DT.CBD.male.test <- ggplot(graph.DT_CBD.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.CBD.male.train <- ggplot(graph.DT_CBD.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.CBD.male.comb <- plot.DT.CBD.male.train + plot.DT.CBD.male.test + 
  plot_annotation(title = "DT-CBD Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
print(Plot.DT.CBD.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.DT_CBD.fem) {
  keep <- rep(TRUE, nrow(graph.DT_CBD.fem))
  for (i in 1:nrow(graph.DT_CBD.fem)) {
    for (j in 1:nrow(graph.DT_CBD.fem)) {
      if (i != j) {
        if (graph.DT_CBD.fem$rmse.train[j] <= graph.DT_CBD.fem$rmse.train[i] &&
            graph.DT_CBD.fem$rmse.test[j]  <= graph.DT_CBD.fem$rmse.test[i] &&
            (graph.DT_CBD.fem$rmse.train[j] < graph.DT_CBD.fem$rmse.train[i] ||
             graph.DT_CBD.fem$rmse.test[j]  < graph.DT_CBD.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_CBD.fem$pareto <- pareto_front(graph.DT_CBD.fem)

Plot.EFF.DT.CBD.fem <- ggplot(graph.DT_CBD.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_CBD.fem[graph.DT_CBD.fem$pareto, ][order(graph.DT_CBD.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_CBD.fem[graph.DT_CBD.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-CBD Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.DT_CBD.male) {
  keep <- rep(TRUE, nrow(graph.DT_CBD.male))
  for (i in 1:nrow(graph.DT_CBD.male)) {
    for (j in 1:nrow(graph.DT_CBD.male)) {
      if (i != j) {
        if (graph.DT_CBD.male$rmse.train[j] <= graph.DT_CBD.male$rmse.train[i] &&
            graph.DT_CBD.male$rmse.test[j]  <= graph.DT_CBD.male$rmse.test[i] &&
            (graph.DT_CBD.male$rmse.train[j] < graph.DT_CBD.male$rmse.train[i] ||
             graph.DT_CBD.male$rmse.test[j]  < graph.DT_CBD.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_CBD.male$pareto <- pareto_front(graph.DT_CBD.male)

Plot.EFF.DT.CBD.male <- ggplot(graph.DT_CBD.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_CBD.male[graph.DT_CBD.male$pareto, ][order(graph.DT_CBD.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_CBD.male[graph.DT_CBD.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-CBD Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.DT.CBD.comb <- Plot.EFF.DT.CBD.fem + Plot.EFF.DT.CBD.male 
print(Plot.EFF.DT.CBD.comb)
