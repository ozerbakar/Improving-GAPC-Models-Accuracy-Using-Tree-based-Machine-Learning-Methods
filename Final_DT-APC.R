
Deaths.test.APC <- Deaths.test
Deaths.test.APC$psi.APC <- 0

# Creating learner
dt <- makeLearner("regr.rpart", predict.type = "response")

# Creating training and testing tasks
trainTask.DT.APC <- makeRegrTask(data = Deaths.APC, target = "psi.APC")
testTask.DT.APC <- makeRegrTask(data = Deaths.test.APC, target = "psi.APC")

# Creating parameters grid
dt_params <- expand.grid(
  cp = 0.01,
  minsplit = c(1: 10),
  maxdepth = c(1:10),
  minbucket = c(1:10)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 2L)

# Create outputs for saving estimations
output_APC.train <- Deaths.APC
output_APC.test <- Deaths.test.APC

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
    task = trainTask.DT.APC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model <- train(pars, trainTask.DT.APC)
  
  # Prediction of the model
  preds.test <- predict(model, task = testTask.DT.APC)
  preds.train <- predict(model, task = trainTask.DT.APC)
  
  # Saving predictions
  output_APC.test[[paste0("hp ",i)]] <- preds.test$data$response
  output_APC.train[[paste0("hp ",i)]] <- preds.train$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.APC <- as.vector(as.matrix(for.m.APC.fem))
Rate.test.male.APC <- as.vector(as.matrix(for.m.APC.male))

test.psi.fem.APC <- output_APC.test %>% filter(Gender == "f")
test.psi.fem.APC <- test.psi.fem.APC[-1:-5]
test.psi.male.APC <- output_APC.test %>% filter(Gender == "m")
test.psi.male.APC <- test.psi.male.APC[-1:-5]

test.imp.rates.fem.APC <- as.data.frame(as.matrix(test.psi.fem.APC)*Rate.test.fem.APC)
test.imp.rates.fem.APC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.APC <- as.data.frame(as.matrix(test.psi.male.APC)*Rate.test.male.APC)
test.imp.rates.male.APC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.APC.fem <- c()
test.rmse_results.APC.male <- c()

for (i in colnames(test.imp.rates.fem.APC)) {
  if (i != "obs.test") {
    test.rmse_val.APC.fem <- RMSE(test.imp.rates.fem.APC[[i]], test.imp.rates.fem.APC$obs.test)
    test.rmse_results.APC.fem[i] <- test.rmse_val.APC.fem
  }
}

for (i in colnames(test.imp.rates.male.APC)) {
  if (i != "obs.test") {
    test.rmse_val.APC.male <- RMSE(test.imp.rates.male.APC[[i]], test.imp.rates.male.APC$obs.test)
    test.rmse_results.APC.male[i] <- test.rmse_val.APC.male
  }
}

test.rmse.APC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.APC.fem),
  rmse.test = as.numeric(test.rmse_results.APC.fem)
)
test.rmse.APC.male <- data.frame(
  Hyper.p = names(test.rmse_results.APC.male),
  rmse.test = as.numeric(test.rmse_results.APC.male)
)

test.rmse.lower.APC.fem <- test.rmse.APC.fem %>% filter(rmse.test < rmse.for.APC.fem)
test.rmse.lower.APC.male <- test.rmse.APC.male %>% filter(rmse.test < rmse.for.APC.male)

Rate.train.fem.APC <- as.vector(as.matrix(m.APC.fem))
Rate.train.male.APC <- as.vector(as.matrix(m.APC.male))

train.psi.fem.APC <- output_APC.train %>% filter(Gender == "f")
train.psi.fem.APC <- train.psi.fem.APC[-1:-5]
train.psi.male.APC <- output_APC.train %>% filter(Gender == "m")
train.psi.male.APC <- train.psi.male.APC[-1:-5]

train.imp.rates.fem.APC <- as.data.frame(as.matrix(train.psi.fem.APC)*Rate.train.fem.APC)
train.imp.rates.fem.APC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.APC <- as.data.frame(as.matrix(train.psi.male.APC)*Rate.train.male.APC)
train.imp.rates.male.APC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.APC.fem <- c()
train.rmse_results.APC.male <- c()

for (i in colnames(train.imp.rates.fem.APC)) {
  if (i != "obs.train") {
    train.rmse_val.APC.fem <- RMSE(train.imp.rates.fem.APC[[i]], train.imp.rates.fem.APC$obs.train)
    train.rmse_results.APC.fem[i] <- train.rmse_val.APC.fem
  }
}
for (i in colnames(train.imp.rates.male.APC)) {
  if (i != "obs.train") {
    train.rmse_val.APC.male <- RMSE(train.imp.rates.male.APC[[i]], train.imp.rates.male.APC$obs.train)
    train.rmse_results.APC.male[i] <- train.rmse_val.APC.male
  }
}
train.rmse.APC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.APC.fem),
  rmse.train = as.numeric(train.rmse_results.APC.fem))
train.rmse.APC.male <- data.frame(
  Hyper.p = names(train.rmse_results.APC.male),
  rmse.train = as.numeric(train.rmse_results.APC.male))

train.rmse.lower.APC.fem <- train.rmse.APC.fem %>% filter(rmse.train < rmse.fit.APC.fem)
tt.rmse.APC.fem <- inner_join(test.rmse.lower.APC.fem, train.rmse.lower.APC.fem, by = "Hyper.p")
train.rmse.lower.APC.male <- train.rmse.APC.male %>% filter(rmse.train < rmse.fit.APC.male)
tt.rmse.APC.male <- inner_join(test.rmse.lower.APC.male, train.rmse.lower.APC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.APC.fem$rmse.test <- round(tt.rmse.APC.fem$rmse.test, digits = 8)
tt.rmse.APC.fem$rmse.train <- round(tt.rmse.APC.fem$rmse.train, digits = 8)
tt.rmse.APC.male$rmse.test <- round(tt.rmse.APC.male$rmse.test, digits = 8)
tt.rmse.APC.male$rmse.train <- round(tt.rmse.APC.male$rmse.train, digits = 8)

graph.DT_APC.fem <- tt.rmse.APC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.DT_APC.male <- tt.rmse.APC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.DT_APC.fem <- graph.DT_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.DT_APC.male <- graph.DT_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.DT_APC.fem <- graph.DT_APC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.DT_APC.male <- graph.DT_APC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.DT_APC.fem <- graph.DT_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.DT_APC.male <- graph.DT_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.DT_APC.fem$Hyper.p <- factor(graph.DT_APC.fem$Hyper.p,
                                  levels = graph.DT_APC.fem$Hyper.p[order(as.numeric(str_extract(graph.DT_APC.fem$Hyper.p, "[0-9]+")))])
graph.DT_APC.male$Hyper.p <- factor(graph.DT_APC.male$Hyper.p,
                                   levels = graph.DT_APC.male$Hyper.p[order(as.numeric(str_extract(graph.DT_APC.male$Hyper.p, "[0-9]+")))])

plot.DT.APC.fem.test <- ggplot(graph.DT_APC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.APC.fem.train <- ggplot(graph.DT_APC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.APC.fem.comb <- plot.DT.APC.fem.train + plot.DT.APC.fem.test + 
  plot_annotation(title = "DT-APC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))
print(Plot.DT.APC.fem.comb)

plot.DT.APC.male.test <- ggplot(graph.DT_APC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.APC.male.train <- ggplot(graph.DT_APC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.APC.male.comb <- plot.DT.APC.male.train + plot.DT.APC.male.test + 
  plot_annotation(title = "DT-APC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.DT.APC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.DT_APC.fem) {
  keep <- rep(TRUE, nrow(graph.DT_APC.fem))
  for (i in 1:nrow(graph.DT_APC.fem)) {
    for (j in 1:nrow(graph.DT_APC.fem)) {
      if (i != j) {
        if (graph.DT_APC.fem$rmse.train[j] <= graph.DT_APC.fem$rmse.train[i] &&
            graph.DT_APC.fem$rmse.test[j]  <= graph.DT_APC.fem$rmse.test[i] &&
            (graph.DT_APC.fem$rmse.train[j] < graph.DT_APC.fem$rmse.train[i] ||
             graph.DT_APC.fem$rmse.test[j]  < graph.DT_APC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_APC.fem$pareto <- pareto_front(graph.DT_APC.fem)

Plot.EFF.DT.APC.fem <- ggplot(graph.DT_APC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_APC.fem[graph.DT_APC.fem$pareto, ][order(graph.DT_APC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_APC.fem[graph.DT_APC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-APC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.DT_APC.male) {
  keep <- rep(TRUE, nrow(graph.DT_APC.male))
  for (i in 1:nrow(graph.DT_APC.male)) {
    for (j in 1:nrow(graph.DT_APC.male)) {
      if (i != j) {
        if (graph.DT_APC.male$rmse.train[j] <= graph.DT_APC.male$rmse.train[i] &&
            graph.DT_APC.male$rmse.test[j]  <= graph.DT_APC.male$rmse.test[i] &&
            (graph.DT_APC.male$rmse.train[j] < graph.DT_APC.male$rmse.train[i] ||
             graph.DT_APC.male$rmse.test[j]  < graph.DT_APC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_APC.male$pareto <- pareto_front(graph.DT_APC.male)

Plot.EFF.DT.APC.male <- ggplot(graph.DT_APC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_APC.male[graph.DT_APC.male$pareto, ][order(graph.DT_APC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_APC.male[graph.DT_APC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-APC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.DT.APC.comb <- Plot.EFF.DT.APC.fem + Plot.EFF.DT.APC.male 
print(Plot.EFF.DT.APC.comb)
