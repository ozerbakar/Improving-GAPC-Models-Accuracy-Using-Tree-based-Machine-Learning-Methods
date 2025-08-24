
Deaths.test.GB.CBD <- Deaths.test
Deaths.test.GB.CBD$psi.CBD <- 0

# Creating learner
gb <- makeLearner("regr.gbm", predict.type = "response")

# Creating training and testing tasks
trainTask.GB.CBD <- makeRegrTask(data = Deaths.CBD, target = "psi.CBD")
testTask.GB.CBD <- makeRegrTask(data = Deaths.test.GB.CBD, target = "psi.CBD")

# Creating parameters grid
gb_params <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  n.trees = c(50, 100, 150),
  interaction.depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_GB.CBD.train <- Deaths.CBD
output_GB.CBD.test <- Deaths.test.GB.CBD

# Calibration of psi over all hyper-parameter combinations
for (i in 1:nrow(gb_params)) {
  hp.gb <- gb_params[i, ]
  
  # Selecting hyper-parameter set
  pars.gb <- setHyperPars(
    learner = gb,
    par.vals = list(
      shrinkage = hp.gb$shrinkage,
      n.trees = hp.gb$n.trees,
      interaction.depth = hp.gb$interaction.depth
    )
  )
  
  # Cross-validation
  resample_result <- resample(
    learner = pars.gb,
    task = trainTask.GB.CBD,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.gb <- train(pars.gb, trainTask.GB.CBD)
  
  # Prediction of the model
  preds.test.gb <- predict(model.gb, task = testTask.GB.CBD)
  preds.train.gb <- predict(model.gb, task = trainTask.GB.CBD)
  
  # Saving predictions
  output_GB.CBD.test[[paste0("hp ",i)]] <- preds.test.gb$data$response
  output_GB.CBD.train[[paste0("hp ",i)]] <- preds.train.gb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.CBD <- as.vector(as.matrix(for.m.CBD.fem))
Rate.test.male.CBD <- as.vector(as.matrix(for.m.CBD.male))

test.psi.fem.GB.CBD <- output_GB.CBD.test %>% filter(Gender == "f")
test.psi.fem.GB.CBD <- test.psi.fem.GB.CBD[-1:-5]
test.psi.male.GB.CBD <- output_GB.CBD.test %>% filter(Gender == "m")
test.psi.male.GB.CBD <- test.psi.male.GB.CBD[-1:-5]

test.imp.rates.fem.GB.CBD <- as.data.frame(as.matrix(test.psi.fem.GB.CBD)*Rate.test.fem.CBD)
test.imp.rates.fem.GB.CBD$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.GB.CBD <- as.data.frame(as.matrix(test.psi.male.GB.CBD)*Rate.test.male.CBD)
test.imp.rates.male.GB.CBD$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.GB.CBD.fem <- c()
test.rmse_results.GB.CBD.male <- c()

for (i in colnames(test.imp.rates.fem.GB.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.GB.CBD.fem <- RMSE(test.imp.rates.fem.GB.CBD[[i]], test.imp.rates.fem.GB.CBD$obs.test)
    test.rmse_results.GB.CBD.fem[i] <- test.rmse_val.GB.CBD.fem
  }
}

for (i in colnames(test.imp.rates.male.GB.CBD)) {
  if (i != "obs.test") {
    test.rmse_val.GB.CBD.male <- RMSE(test.imp.rates.male.GB.CBD[[i]], test.imp.rates.male.GB.CBD$obs.test)
    test.rmse_results.GB.CBD.male[i] <- test.rmse_val.GB.CBD.male
  }
}

test.rmse.GB.CBD.fem <- data.frame(
  Hyper.p = names(test.rmse_results.GB.CBD.fem),
  rmse.test = as.numeric(test.rmse_results.GB.CBD.fem)
)
test.rmse.GB.CBD.male <- data.frame(
  Hyper.p = names(test.rmse_results.GB.CBD.male),
  rmse.test = as.numeric(test.rmse_results.GB.CBD.male)
)

test.rmse.lower.GB.CBD.fem <- test.rmse.GB.CBD.fem %>% filter(rmse.test < rmse.for.CBD.fem)
test.rmse.lower.GB.CBD.male <- test.rmse.GB.CBD.male %>% filter(rmse.test < rmse.for.CBD.male)

Rate.train.fem.CBD <- as.vector(as.matrix(m.CBD.fem))
Rate.train.male.CBD <- as.vector(as.matrix(m.CBD.male))

train.psi.fem.GB.CBD <- output_GB.CBD.train %>% filter(Gender == "f")
train.psi.fem.GB.CBD <- train.psi.fem.GB.CBD[-1:-5]
train.psi.male.GB.CBD <- output_GB.CBD.train %>% filter(Gender == "m")
train.psi.male.GB.CBD <- train.psi.male.GB.CBD[-1:-5]

train.imp.rates.fem.GB.CBD <- as.data.frame(as.matrix(train.psi.fem.GB.CBD)*Rate.train.fem.CBD)
train.imp.rates.fem.GB.CBD$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.GB.CBD <- as.data.frame(as.matrix(train.psi.male.GB.CBD)*Rate.train.male.CBD)
train.imp.rates.male.GB.CBD$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.GB.CBD.fem <- c()
train.rmse_results.GB.CBD.male <- c()

for (i in colnames(train.imp.rates.fem.GB.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.GB.CBD.fem <- RMSE(train.imp.rates.fem.GB.CBD[[i]], train.imp.rates.fem.GB.CBD$obs.train)
    train.rmse_results.GB.CBD.fem[i] <- train.rmse_val.GB.CBD.fem
  }
}
for (i in colnames(train.imp.rates.male.GB.CBD)) {
  if (i != "obs.train") {
    train.rmse_val.GB.CBD.male <- RMSE(train.imp.rates.male.GB.CBD[[i]], train.imp.rates.male.GB.CBD$obs.train)
    train.rmse_results.GB.CBD.male[i] <- train.rmse_val.GB.CBD.male
  }
}
train.rmse.GB.CBD.fem <- data.frame(
  Hyper.p = names(train.rmse_results.GB.CBD.fem),
  rmse.train = as.numeric(train.rmse_results.GB.CBD.fem))
train.rmse.GB.CBD.male <- data.frame(
  Hyper.p = names(train.rmse_results.GB.CBD.male),
  rmse.train = as.numeric(train.rmse_results.GB.CBD.male))

train.rmse.lower.GB.CBD.fem <- train.rmse.GB.CBD.fem %>% filter(rmse.train < rmse.fit.CBD.fem)
tt.rmse.GB.CBD.fem <- inner_join(test.rmse.lower.GB.CBD.fem, train.rmse.lower.GB.CBD.fem, by = "Hyper.p")
train.rmse.lower.GB.CBD.male <- train.rmse.GB.CBD.male %>% filter(rmse.train < rmse.fit.CBD.male)
tt.rmse.GB.CBD.male <- inner_join(test.rmse.lower.GB.CBD.male, train.rmse.lower.GB.CBD.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.GB.CBD.fem$rmse.test <- round(tt.rmse.GB.CBD.fem$rmse.test, digits = 8)
tt.rmse.GB.CBD.fem$rmse.train <- round(tt.rmse.GB.CBD.fem$rmse.train, digits = 8)
tt.rmse.GB.CBD.male$rmse.test <- round(tt.rmse.GB.CBD.male$rmse.test, digits = 8)
tt.rmse.GB.CBD.male$rmse.train <- round(tt.rmse.GB.CBD.male$rmse.train, digits = 8)

graph.GB_CBD.fem <- tt.rmse.GB.CBD.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.GB_CBD.male <- tt.rmse.GB.CBD.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.GB_CBD.fem <- graph.GB_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.GB_CBD.male <- graph.GB_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.GB_CBD.fem <- graph.GB_CBD.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.GB_CBD.male <- graph.GB_CBD.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.GB_CBD.fem <- graph.GB_CBD.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.GB_CBD.male <- graph.GB_CBD.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.GB_CBD.fem$Hyper.p <- factor(graph.GB_CBD.fem$Hyper.p,
                                  levels = graph.GB_CBD.fem$Hyper.p[order(as.numeric(str_extract(graph.GB_CBD.fem$Hyper.p, "[0-9]+")))])
graph.GB_CBD.male$Hyper.p <- factor(graph.GB_CBD.male$Hyper.p,
                                   levels = graph.GB_CBD.male$Hyper.p[order(as.numeric(str_extract(graph.GB_CBD.male$Hyper.p, "[0-9]+")))])

plot.GB.CBD.fem.test <- ggplot(graph.GB_CBD.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.CBD.fem.train <- ggplot(graph.GB_CBD.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.CBD.fem.comb <- plot.GB.CBD.fem.train + plot.GB.CBD.fem.test + 
  plot_annotation(title = "GB-CBD Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.CBD.fem.comb)

plot.GB.CBD.male.test <- ggplot(graph.GB_CBD.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.CBD.male.train <- ggplot(graph.GB_CBD.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.CBD.male.comb <- plot.GB.CBD.male.train + plot.GB.CBD.male.test + 
  plot_annotation(title = "GB-CBD Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.CBD.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.GB_CBD.fem) {
  keep <- rep(TRUE, nrow(graph.GB_CBD.fem))
  for (i in 1:nrow(graph.GB_CBD.fem)) {
    for (j in 1:nrow(graph.GB_CBD.fem)) {
      if (i != j) {
        if (graph.GB_CBD.fem$rmse.train[j] <= graph.GB_CBD.fem$rmse.train[i] &&
            graph.GB_CBD.fem$rmse.test[j]  <= graph.GB_CBD.fem$rmse.test[i] &&
            (graph.GB_CBD.fem$rmse.train[j] < graph.GB_CBD.fem$rmse.train[i] ||
             graph.GB_CBD.fem$rmse.test[j]  < graph.GB_CBD.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_CBD.fem$pareto <- pareto_front(graph.GB_CBD.fem)

# Plot
Plot.EFF.GB.CBD.fem <- ggplot(graph.GB_CBD.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_CBD.fem[graph.GB_CBD.fem$pareto, ][order(graph.GB_CBD.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_CBD.fem[graph.GB_CBD.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-CBD Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.GB_CBD.male) {
  keep <- rep(TRUE, nrow(graph.GB_CBD.male))
  for (i in 1:nrow(graph.GB_CBD.male)) {
    for (j in 1:nrow(graph.GB_CBD.male)) {
      if (i != j) {
        if (graph.GB_CBD.male$rmse.train[j] <= graph.GB_CBD.male$rmse.train[i] &&
            graph.GB_CBD.male$rmse.test[j]  <= graph.GB_CBD.male$rmse.test[i] &&
            (graph.GB_CBD.male$rmse.train[j] < graph.GB_CBD.male$rmse.train[i] ||
             graph.GB_CBD.male$rmse.test[j]  < graph.GB_CBD.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_CBD.male$pareto <- pareto_front(graph.GB_CBD.male)

# Plot
Plot.EFF.GB.CBD.male <- ggplot(graph.GB_CBD.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_CBD.male[graph.GB_CBD.male$pareto, ][order(graph.GB_CBD.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_CBD.male[graph.GB_CBD.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-CBD Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.GB.CBD.comb <- Plot.EFF.GB.CBD.fem + Plot.EFF.GB.CBD.male 
print(Plot.EFF.GB.CBD.comb)
