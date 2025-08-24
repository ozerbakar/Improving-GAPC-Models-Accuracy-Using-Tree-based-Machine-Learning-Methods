
Deaths.test.Plat <- Deaths.test
Deaths.test.Plat$psi.Plat <- 0

# Creating learner
dt <- makeLearner("regr.rpart", predict.type = "response")

# Creating training and testing tasks
trainTask.DT.Plat <- makeRegrTask(data = Deaths.Plat, target = "psi.Plat")
testTask.DT.Plat <- makeRegrTask(data = Deaths.test.Plat, target = "psi.Plat")

# Creating parameters grid
dt_params <- expand.grid(
  cp = 0.001,
  minsplit = c(1: 5),
  maxdepth = c(1:30),
  minbucket = c(10:40)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_Plat.train <- Deaths.Plat
output_Plat.test <- Deaths.test.Plat

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
    task = trainTask.DT.Plat,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model <- train(pars, trainTask.DT.Plat)
  
  # Prediction of the model
  preds.test <- predict(model, task = testTask.DT.Plat)
  preds.train <- predict(model, task = trainTask.DT.Plat)
  
  # Saving predictions
  output_Plat.test[[paste0("hp ",i)]] <- preds.test$data$response
  output_Plat.train[[paste0("hp ",i)]] <- preds.train$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.Plat <- as.vector(as.matrix(for.m.Plat.fem))
Rate.test.male.Plat <- as.vector(as.matrix(for.m.Plat.male))

test.psi.fem.Plat <- output_Plat.test %>% filter(Gender == "f")
test.psi.fem.Plat <- test.psi.fem.Plat[-1:-5]
test.psi.male.Plat <- output_Plat.test %>% filter(Gender == "m")
test.psi.male.Plat <- test.psi.male.Plat[-1:-5]

test.imp.rates.fem.Plat <- as.data.frame(as.matrix(test.psi.fem.Plat)*Rate.test.fem.Plat)
test.imp.rates.fem.Plat$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.Plat <- as.data.frame(as.matrix(test.psi.male.Plat)*Rate.test.male.Plat)
test.imp.rates.male.Plat$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.Plat.fem <- c()
test.rmse_results.Plat.male <- c()

for (i in colnames(test.imp.rates.fem.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.Plat.fem <- RMSE(test.imp.rates.fem.Plat[[i]], test.imp.rates.fem.Plat$obs.test)
    test.rmse_results.Plat.fem[i] <- test.rmse_val.Plat.fem
  }
}

for (i in colnames(test.imp.rates.male.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.Plat.male <- RMSE(test.imp.rates.male.Plat[[i]], test.imp.rates.male.Plat$obs.test)
    test.rmse_results.Plat.male[i] <- test.rmse_val.Plat.male
  }
}

test.rmse.Plat.fem <- data.frame(
  Hyper.p = names(test.rmse_results.Plat.fem),
  rmse.test = as.numeric(test.rmse_results.Plat.fem)
)
test.rmse.Plat.male <- data.frame(
  Hyper.p = names(test.rmse_results.Plat.male),
  rmse.test = as.numeric(test.rmse_results.Plat.male)
)

test.rmse.lower.Plat.fem <- test.rmse.Plat.fem %>% filter(rmse.test < rmse.for.Plat.fem)
test.rmse.lower.Plat.male <- test.rmse.Plat.male %>% filter(rmse.test < rmse.for.Plat.male)

Rate.train.fem.Plat <- as.vector(as.matrix(m.Plat.fem))
Rate.train.male.Plat <- as.vector(as.matrix(m.Plat.male))

train.psi.fem.Plat <- output_Plat.train %>% filter(Gender == "f")
train.psi.fem.Plat <- train.psi.fem.Plat[-1:-5]
train.psi.male.Plat <- output_Plat.train %>% filter(Gender == "m")
train.psi.male.Plat <- train.psi.male.Plat[-1:-5]

train.imp.rates.fem.Plat <- as.data.frame(as.matrix(train.psi.fem.Plat)*Rate.train.fem.Plat)
train.imp.rates.fem.Plat$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.Plat <- as.data.frame(as.matrix(train.psi.male.Plat)*Rate.train.male.Plat)
train.imp.rates.male.Plat$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.Plat.fem <- c()
train.rmse_results.Plat.male <- c()

for (i in colnames(train.imp.rates.fem.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.Plat.fem <- RMSE(train.imp.rates.fem.Plat[[i]], train.imp.rates.fem.Plat$obs.train)
    train.rmse_results.Plat.fem[i] <- train.rmse_val.Plat.fem
  }
}
for (i in colnames(train.imp.rates.male.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.Plat.male <- RMSE(train.imp.rates.male.Plat[[i]], train.imp.rates.male.Plat$obs.train)
    train.rmse_results.Plat.male[i] <- train.rmse_val.Plat.male
  }
}
train.rmse.Plat.fem <- data.frame(
  Hyper.p = names(train.rmse_results.Plat.fem),
  rmse.train = as.numeric(train.rmse_results.Plat.fem))
train.rmse.Plat.male <- data.frame(
  Hyper.p = names(train.rmse_results.Plat.male),
  rmse.train = as.numeric(train.rmse_results.Plat.male))

train.rmse.lower.Plat.fem <- train.rmse.Plat.fem %>% filter(rmse.train < rmse.fit.Plat.fem)
tt.rmse.Plat.fem <- inner_join(test.rmse.lower.Plat.fem, train.rmse.lower.Plat.fem, by = "Hyper.p")
train.rmse.lower.Plat.male <- train.rmse.Plat.male %>% filter(rmse.train < rmse.fit.Plat.male)
tt.rmse.Plat.male <- inner_join(test.rmse.lower.Plat.male, train.rmse.lower.Plat.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.Plat.fem$rmse.test <- round(tt.rmse.Plat.fem$rmse.test, digits = 8)
tt.rmse.Plat.fem$rmse.train <- round(tt.rmse.Plat.fem$rmse.train, digits = 8)
tt.rmse.Plat.male$rmse.test <- round(tt.rmse.Plat.male$rmse.test, digits = 8)
tt.rmse.Plat.male$rmse.train <- round(tt.rmse.Plat.male$rmse.train, digits = 8)

graph.DT_Plat.fem <- tt.rmse.Plat.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.DT_Plat.male <- tt.rmse.Plat.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.DT_Plat.fem <- graph.DT_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.DT_Plat.male <- graph.DT_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.DT_Plat.fem <- graph.DT_Plat.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.DT_Plat.male <- graph.DT_Plat.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.DT_Plat.fem <- graph.DT_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.DT_Plat.male <- graph.DT_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.DT_Plat.fem$Hyper.p <- factor(graph.DT_Plat.fem$Hyper.p,
                                   levels = graph.DT_Plat.fem$Hyper.p[order(as.numeric(str_extract(graph.DT_Plat.fem$Hyper.p, "[0-9]+")))])
graph.DT_Plat.male$Hyper.p <- factor(graph.DT_Plat.male$Hyper.p,
                                   levels = graph.DT_Plat.male$Hyper.p[order(as.numeric(str_extract(graph.DT_Plat.male$Hyper.p, "[0-9]+")))])

plot.DT.Plat.fem.test <- ggplot(graph.DT_Plat.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.Plat.fem.train <- ggplot(graph.DT_Plat.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.Plat.fem.comb <- plot.DT.Plat.fem.train + plot.DT.Plat.fem.test + 
  plot_annotation(title = "DT-Plat Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.DT.Plat.fem.comb)

plot.DT.Plat.male.test <- ggplot(graph.DT_Plat.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.DT.Plat.male.train <- ggplot(graph.DT_Plat.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.DT.Plat.male.comb <- plot.DT.Plat.male.train + plot.DT.Plat.male.test + 
  plot_annotation(title = "DT-Plat Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.DT.Plat.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.DT_Plat.fem) {
  keep <- rep(TRUE, nrow(graph.DT_Plat.fem))
  for (i in 1:nrow(graph.DT_Plat.fem)) {
    for (j in 1:nrow(graph.DT_Plat.fem)) {
      if (i != j) {
        if (graph.DT_Plat.fem$rmse.train[j] <= graph.DT_Plat.fem$rmse.train[i] &&
            graph.DT_Plat.fem$rmse.test[j]  <= graph.DT_Plat.fem$rmse.test[i] &&
            (graph.DT_Plat.fem$rmse.train[j] < graph.DT_Plat.fem$rmse.train[i] ||
             graph.DT_Plat.fem$rmse.test[j]  < graph.DT_Plat.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_Plat.fem$pareto <- pareto_front(graph.DT_Plat.fem)

# Plot
Plot.EFF.DT.Plat.fem <- ggplot(graph.DT_Plat.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_Plat.fem[graph.DT_Plat.fem$pareto, ][order(graph.DT_Plat.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_Plat.fem[graph.DT_Plat.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-Plat Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.DT_Plat.male) {
  keep <- rep(TRUE, nrow(graph.DT_Plat.male))
  for (i in 1:nrow(graph.DT_Plat.male)) {
    for (j in 1:nrow(graph.DT_Plat.male)) {
      if (i != j) {
        if (graph.DT_Plat.male$rmse.train[j] <= graph.DT_Plat.male$rmse.train[i] &&
            graph.DT_Plat.male$rmse.test[j]  <= graph.DT_Plat.male$rmse.test[i] &&
            (graph.DT_Plat.male$rmse.train[j] < graph.DT_Plat.male$rmse.train[i] ||
             graph.DT_Plat.male$rmse.test[j]  < graph.DT_Plat.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.DT_Plat.male$pareto <- pareto_front(graph.DT_Plat.male)

# Plot

Plot.EFF.DT.Plat.male <- ggplot(graph.DT_Plat.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.DT_Plat.male[graph.DT_Plat.male$pareto, ][order(graph.DT_Plat.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.DT_Plat.male[graph.DT_Plat.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "DT-Plat Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.DT.Plat.comb <- Plot.EFF.DT.Plat.fem + Plot.EFF.DT.Plat.male 
print(Plot.EFF.DT.Plat.comb)
