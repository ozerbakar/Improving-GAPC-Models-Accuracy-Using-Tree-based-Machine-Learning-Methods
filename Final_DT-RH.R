
Deaths.test.RH <- Deaths.test
Deaths.test.RH$psi.RH <- 0

library(mlr)
# Creating learner
dt <- makeLearner("regr.rpart", predict.type = "response")

# Creating training and testing tasks
trainTask.DT.RH <- makeRegrTask(data = Deaths.RH, target = "psi.RH")
testTask.DT.RH <- makeRegrTask(data = Deaths.test.RH, target = "psi.RH")

# Creating parameters grid
dt_params <- expand.grid(
  cp = 0.001,
  minsplit = c(1: 10),
  maxdepth = c(1:30),
  minbucket = c(1:20)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_RH.train <- Deaths.RH
output_RH.test <- Deaths.test.RH

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
    task = trainTask.DT.RH,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model <- train(pars, trainTask.DT.RH)
  
  # Prediction of the model
  preds.test <- predict(model, task = testTask.DT.RH)
  preds.train <- predict(model, task = trainTask.DT.RH)
  
  # Saving predictions
  output_RH.test[[paste0("hp ",i)]] <- preds.test$data$response
  output_RH.train[[paste0("hp ",i)]] <- preds.train$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.RH <- as.vector(as.matrix(for.m.RH.fem))
Rate.test.male.RH <- as.vector(as.matrix(for.m.RH.male))

test.psi.fem.RH <- output_RH.test %>% filter(Gender == "f")
test.psi.fem.RH <- test.psi.fem.RH[-1:-5]
test.psi.male.RH <- output_RH.test %>% filter(Gender == "m")
test.psi.male.RH <- test.psi.male.RH[-1:-5]

test.imp.rates.fem.RH <- as.data.frame(as.matrix(test.psi.fem.RH)*Rate.test.fem.RH)
test.imp.rates.fem.RH$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.RH <- as.data.frame(as.matrix(test.psi.male.RH)*Rate.test.male.RH)
test.imp.rates.male.RH$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.RH.fem <- c()
test.rmse_results.RH.male <- c()

for (i in colnames(test.imp.rates.fem.RH)) {
  if (i != "obs.test") {
    test.rmse_val.RH.fem <- RMSE(test.imp.rates.fem.RH[[i]], test.imp.rates.fem.RH$obs.test)
    test.rmse_results.RH.fem[i] <- test.rmse_val.RH.fem
  }
}

for (i in colnames(test.imp.rates.male.RH)) {
  if (i != "obs.test") {
    test.rmse_val.RH.male <- RMSE(test.imp.rates.male.RH[[i]], test.imp.rates.male.RH$obs.test)
    test.rmse_results.RH.male[i] <- test.rmse_val.RH.male
  }
}

test.rmse.RH.fem <- data.frame(
  Hyper.p = names(test.rmse_results.RH.fem),
  rmse.test = as.numeric(test.rmse_results.RH.fem)
)
test.rmse.RH.male <- data.frame(
  Hyper.p = names(test.rmse_results.RH.male),
  rmse.test = as.numeric(test.rmse_results.RH.male)
)

test.rmse.lower.RH.fem <- test.rmse.RH.fem %>% filter(rmse.test < rmse.for.RH.fem)
test.rmse.lower.RH.male <- test.rmse.RH.male %>% filter(rmse.test < rmse.for.RH.male)

Rate.train.fem.RH <- as.vector(as.matrix(m.RH.fem))
Rate.train.male.RH <- as.vector(as.matrix(m.RH.male))

train.psi.fem.RH <- output_RH.train %>% filter(Gender == "f")
train.psi.fem.RH <- train.psi.fem.RH[-1:-5]
train.psi.male.RH <- output_RH.train %>% filter(Gender == "m")
train.psi.male.RH <- train.psi.male.RH[-1:-5]

train.imp.rates.fem.RH <- as.data.frame(as.matrix(train.psi.fem.RH)*Rate.train.fem.RH)
train.imp.rates.fem.RH$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.RH <- as.data.frame(as.matrix(train.psi.male.RH)*Rate.train.male.RH)
train.imp.rates.male.RH$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.RH.fem <- c()
train.rmse_results.RH.male <- c()

for (i in colnames(train.imp.rates.fem.RH)) {
  if (i != "obs.train") {
    train.rmse_val.RH.fem <- RMSE(train.imp.rates.fem.RH[[i]], train.imp.rates.fem.RH$obs.train)
    train.rmse_results.RH.fem[i] <- train.rmse_val.RH.fem
  }
}
for (i in colnames(train.imp.rates.male.RH)) {
  if (i != "obs.train") {
    train.rmse_val.RH.male <- RMSE(train.imp.rates.male.RH[[i]], train.imp.rates.male.RH$obs.train)
    train.rmse_results.RH.male[i] <- train.rmse_val.RH.male
  }
}
train.rmse.RH.fem <- data.frame(
  Hyper.p = names(train.rmse_results.RH.fem),
  rmse.train = as.numeric(train.rmse_results.RH.fem))
train.rmse.RH.male <- data.frame(
  Hyper.p = names(train.rmse_results.RH.male),
  rmse.train = as.numeric(train.rmse_results.RH.male))

train.rmse.lower.RH.fem <- train.rmse.RH.fem %>% filter(rmse.train < rmse.fit.RH.fem)
tt.rmse.RH.fem <- inner_join(test.rmse.lower.RH.fem, train.rmse.lower.RH.fem, by = "Hyper.p")
train.rmse.lower.RH.male <- train.rmse.RH.male %>% filter(rmse.train < rmse.fit.RH.male)
tt.rmse.RH.male <- inner_join(test.rmse.lower.RH.male, train.rmse.lower.RH.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.RH.fem$rmse.test <- round(tt.rmse.RH.fem$rmse.test, digits = 8)
tt.rmse.RH.fem$rmse.train <- round(tt.rmse.RH.fem$rmse.train, digits = 8)
tt.rmse.RH.male$rmse.test <- round(tt.rmse.RH.male$rmse.test, digits = 8)
tt.rmse.RH.male$rmse.train <- round(tt.rmse.RH.male$rmse.train, digits = 8)

graph.DT_RH.fem <- tt.rmse.RH.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.DT_RH.male <- tt.rmse.RH.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.DT_RH.fem <- graph.DT_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.DT_RH.male <- graph.DT_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.DT_RH.fem <- graph.DT_RH.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.DT_RH.male <- graph.DT_RH.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.DT_RH.fem <- graph.DT_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.DT_RH.male <- graph.DT_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.DT_RH.fem$Hyper.p <- factor(graph.DT_RH.fem$Hyper.p,
                                   levels = graph.DT_RH.fem$Hyper.p[order(as.numeric(str_extract(graph.DT_RH.fem$Hyper.p, "[0-9]+")))])
graph.DT_RH.male$Hyper.p <- factor(graph.DT_RH.male$Hyper.p,
                                    levels = graph.DT_RH.male$Hyper.p[order(as.numeric(str_extract(graph.DT_RH.male$Hyper.p, "[0-9]+")))])

plot.DT.RH.fem.test <- ggplot(graph.DT_RH.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.RH.fem.train <- ggplot(graph.DT_RH.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.RH.fem.comb <- plot.DT.RH.fem.train + plot.DT.RH.fem.test + 
  plot_annotation(title = "DT-RH Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.DT.RH.fem.comb)

plot.DT.RH.male.test <- ggplot(graph.DT_RH.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.RH.male.train <- ggplot(graph.DT_RH.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.RH.male.comb <- plot.DT.RH.male.train + plot.DT.RH.male.test + 
  plot_annotation(title = "DT-RH Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.DT.RH.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.DT_RH.fem) {
  keep <- rep(TRUE, nrow(graph.DT_RH.fem))
  for (i in 1:nrow(graph.DT_RH.fem)) {
    for (j in 1:nrow(graph.DT_RH.fem)) {
      if (i != j) {
        if (graph.DT_RH.fem$rmse.train[j] <= graph.DT_RH.fem$rmse.train[i] &&
            graph.DT_RH.fem$rmse.test[j]  <= graph.DT_RH.fem$rmse.test[i] &&
            (graph.DT_RH.fem$rmse.train[j] < graph.DT_RH.fem$rmse.train[i] ||
             graph.DT_RH.fem$rmse.test[j]  < graph.DT_RH.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_RH.fem$pareto <- pareto_front(graph.DT_RH.fem)

Plot.EFF.DT.RH.fem <- ggplot(graph.DT_RH.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_RH.fem[graph.DT_RH.fem$pareto, ][order(graph.DT_RH.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_RH.fem[graph.DT_RH.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-RH Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.DT_RH.male) {
  keep <- rep(TRUE, nrow(graph.DT_RH.male))
  for (i in 1:nrow(graph.DT_RH.male)) {
    for (j in 1:nrow(graph.DT_RH.male)) {
      if (i != j) {
        if (graph.DT_RH.male$rmse.train[j] <= graph.DT_RH.male$rmse.train[i] &&
            graph.DT_RH.male$rmse.test[j]  <= graph.DT_RH.male$rmse.test[i] &&
            (graph.DT_RH.male$rmse.train[j] < graph.DT_RH.male$rmse.train[i] ||
             graph.DT_RH.male$rmse.test[j]  < graph.DT_RH.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_RH.male$pareto <- pareto_front(graph.DT_RH.male)

Plot.EFF.DT.RH.male <- ggplot(graph.DT_RH.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_RH.male[graph.DT_RH.male$pareto, ][order(graph.DT_RH.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_RH.male[graph.DT_RH.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-RH Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.DT.RH.comb <- Plot.EFF.DT.RH.fem + Plot.EFF.DT.RH.male 
print(Plot.EFF.DT.RH.comb)
