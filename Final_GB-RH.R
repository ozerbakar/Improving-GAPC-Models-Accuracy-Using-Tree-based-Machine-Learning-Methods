
Deaths.test.GB.RH <- Deaths.test
Deaths.test.GB.RH$psi.RH <- 0

# Creating learner
gb <- makeLearner("regr.gbm", predict.type = "response")

# Creating training and testing tasks
trainTask.GB.RH <- makeRegrTask(data = Deaths.RH, target = "psi.RH")
testTask.GB.RH <- makeRegrTask(data = Deaths.test.GB.RH, target = "psi.RH")

# Creating parameters grid
gb_params <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  n.trees = c(50, 100, 150),
  interaction.depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_GB.RH.train <- Deaths.RH
output_GB.RH.test <- Deaths.test.GB.RH

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
    task = trainTask.GB.RH,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.gb <- train(pars.gb, trainTask.GB.RH)
  
  # Prediction of the model
  preds.test.gb <- predict(model.gb, task = testTask.GB.RH)
  preds.train.gb <- predict(model.gb, task = trainTask.GB.RH)
  
  # Saving predictions
  output_GB.RH.test[[paste0("hp ",i)]] <- preds.test.gb$data$response
  output_GB.RH.train[[paste0("hp ",i)]] <- preds.train.gb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.RH <- as.vector(as.matrix(for.m.RH.fem))
Rate.test.male.RH <- as.vector(as.matrix(for.m.RH.male))

test.psi.fem.GB.RH <- output_GB.RH.test %>% filter(Gender == "f")
test.psi.fem.GB.RH <- test.psi.fem.GB.RH[-1:-5]
test.psi.male.GB.RH <- output_GB.RH.test %>% filter(Gender == "m")
test.psi.male.GB.RH <- test.psi.male.GB.RH[-1:-5]

test.imp.rates.fem.GB.RH <- as.data.frame(as.matrix(test.psi.fem.GB.RH)*Rate.test.fem.RH)
test.imp.rates.fem.GB.RH$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.GB.RH <- as.data.frame(as.matrix(test.psi.male.GB.RH)*Rate.test.male.RH)
test.imp.rates.male.GB.RH$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.GB.RH.fem <- c()
test.rmse_results.GB.RH.male <- c()

for (i in colnames(test.imp.rates.fem.GB.RH)) {
  if (i != "obs.test") {
    test.rmse_val.GB.RH.fem <- RMSE(test.imp.rates.fem.GB.RH[[i]], test.imp.rates.fem.GB.RH$obs.test)
    test.rmse_results.GB.RH.fem[i] <- test.rmse_val.GB.RH.fem
  }
}

for (i in colnames(test.imp.rates.male.GB.RH)) {
  if (i != "obs.test") {
    test.rmse_val.GB.RH.male <- RMSE(test.imp.rates.male.GB.RH[[i]], test.imp.rates.male.GB.RH$obs.test)
    test.rmse_results.GB.RH.male[i] <- test.rmse_val.GB.RH.male
  }
}

test.rmse.GB.RH.fem <- data.frame(
  Hyper.p = names(test.rmse_results.GB.RH.fem),
  rmse.test = as.numeric(test.rmse_results.GB.RH.fem)
)
test.rmse.GB.RH.male <- data.frame(
  Hyper.p = names(test.rmse_results.GB.RH.male),
  rmse.test = as.numeric(test.rmse_results.GB.RH.male)
)

test.rmse.lower.GB.RH.fem <- test.rmse.GB.RH.fem %>% filter(rmse.test < rmse.for.RH.fem)
test.rmse.lower.GB.RH.male <- test.rmse.GB.RH.male %>% filter(rmse.test < rmse.for.RH.male)

Rate.train.fem.RH <- as.vector(as.matrix(m.RH.fem))
Rate.train.male.RH <- as.vector(as.matrix(m.RH.male))

train.psi.fem.GB.RH <- output_GB.RH.train %>% filter(Gender == "f")
train.psi.fem.GB.RH <- train.psi.fem.GB.RH[-1:-5]
train.psi.male.GB.RH <- output_GB.RH.train %>% filter(Gender == "m")
train.psi.male.GB.RH <- train.psi.male.GB.RH[-1:-5]

train.imp.rates.fem.GB.RH <- as.data.frame(as.matrix(train.psi.fem.GB.RH)*Rate.train.fem.RH)
train.imp.rates.fem.GB.RH$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.GB.RH <- as.data.frame(as.matrix(train.psi.male.GB.RH)*Rate.train.male.RH)
train.imp.rates.male.GB.RH$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.GB.RH.fem <- c()
train.rmse_results.GB.RH.male <- c()

for (i in colnames(train.imp.rates.fem.GB.RH)) {
  if (i != "obs.train") {
    train.rmse_val.GB.RH.fem <- RMSE(train.imp.rates.fem.GB.RH[[i]], train.imp.rates.fem.GB.RH$obs.train)
    train.rmse_results.GB.RH.fem[i] <- train.rmse_val.GB.RH.fem
  }
}
for (i in colnames(train.imp.rates.male.GB.RH)) {
  if (i != "obs.train") {
    train.rmse_val.GB.RH.male <- RMSE(train.imp.rates.male.GB.RH[[i]], train.imp.rates.male.GB.RH$obs.train)
    train.rmse_results.GB.RH.male[i] <- train.rmse_val.GB.RH.male
  }
}
train.rmse.GB.RH.fem <- data.frame(
  Hyper.p = names(train.rmse_results.GB.RH.fem),
  rmse.train = as.numeric(train.rmse_results.GB.RH.fem))
train.rmse.GB.RH.male <- data.frame(
  Hyper.p = names(train.rmse_results.GB.RH.male),
  rmse.train = as.numeric(train.rmse_results.GB.RH.male))

train.rmse.lower.GB.RH.fem <- train.rmse.GB.RH.fem %>% filter(rmse.train < rmse.fit.RH.fem)
tt.rmse.GB.RH.fem <- inner_join(test.rmse.lower.GB.RH.fem, train.rmse.lower.GB.RH.fem, by = "Hyper.p")
train.rmse.lower.GB.RH.male <- train.rmse.GB.RH.male %>% filter(rmse.train < rmse.fit.RH.male)
tt.rmse.GB.RH.male <- inner_join(test.rmse.lower.GB.RH.male, train.rmse.lower.GB.RH.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.GB.RH.fem$rmse.test <- round(tt.rmse.GB.RH.fem$rmse.test, digits = 8)
tt.rmse.GB.RH.fem$rmse.train <- round(tt.rmse.GB.RH.fem$rmse.train, digits = 8)
tt.rmse.GB.RH.male$rmse.test <- round(tt.rmse.GB.RH.male$rmse.test, digits = 8)
tt.rmse.GB.RH.male$rmse.train <- round(tt.rmse.GB.RH.male$rmse.train, digits = 8)

graph.GB_RH.fem <- tt.rmse.GB.RH.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.GB_RH.male <- tt.rmse.GB.RH.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.GB_RH.fem <- graph.GB_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.GB_RH.male <- graph.GB_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.GB_RH.fem <- graph.GB_RH.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.GB_RH.male <- graph.GB_RH.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.GB_RH.fem <- graph.GB_RH.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.GB_RH.male <- graph.GB_RH.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.GB_RH.fem$Hyper.p <- factor(graph.GB_RH.fem$Hyper.p,
                                   levels = graph.GB_RH.fem$Hyper.p[order(as.numeric(str_extract(graph.GB_RH.fem$Hyper.p, "[0-9]+")))])
graph.GB_RH.male$Hyper.p <- factor(graph.GB_RH.male$Hyper.p,
                                    levels = graph.GB_RH.male$Hyper.p[order(as.numeric(str_extract(graph.GB_RH.male$Hyper.p, "[0-9]+")))])

plot.GB.RH.fem.test <- ggplot(graph.GB_RH.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.RH.fem.train <- ggplot(graph.GB_RH.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.RH.fem.comb <- plot.GB.RH.fem.train + plot.GB.RH.fem.test + 
  plot_annotation(title = "GB-RH Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.RH.fem.comb)

plot.GB.RH.male.test <- ggplot(graph.GB_RH.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.RH.male.train <- ggplot(graph.GB_RH.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.RH.male.comb <- plot.GB.RH.male.train + plot.GB.RH.male.test + 
  plot_annotation(title = "GB-RH Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.RH.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.GB_RH.fem) {
  keep <- rep(TRUE, nrow(graph.GB_RH.fem))
  for (i in 1:nrow(graph.GB_RH.fem)) {
    for (j in 1:nrow(graph.GB_RH.fem)) {
      if (i != j) {
        if (graph.GB_RH.fem$rmse.train[j] <= graph.GB_RH.fem$rmse.train[i] &&
            graph.GB_RH.fem$rmse.test[j]  <= graph.GB_RH.fem$rmse.test[i] &&
            (graph.GB_RH.fem$rmse.train[j] < graph.GB_RH.fem$rmse.train[i] ||
             graph.GB_RH.fem$rmse.test[j]  < graph.GB_RH.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_RH.fem$pareto <- pareto_front(graph.GB_RH.fem)

# Plot
Plot.EFF.GB.RH.fem <- ggplot(graph.GB_RH.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_RH.fem[graph.GB_RH.fem$pareto, ][order(graph.GB_RH.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_RH.fem[graph.GB_RH.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-RH Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.GB_RH.male) {
  keep <- rep(TRUE, nrow(graph.GB_RH.male))
  for (i in 1:nrow(graph.GB_RH.male)) {
    for (j in 1:nrow(graph.GB_RH.male)) {
      if (i != j) {
        if (graph.GB_RH.male$rmse.train[j] <= graph.GB_RH.male$rmse.train[i] &&
            graph.GB_RH.male$rmse.test[j]  <= graph.GB_RH.male$rmse.test[i] &&
            (graph.GB_RH.male$rmse.train[j] < graph.GB_RH.male$rmse.train[i] ||
             graph.GB_RH.male$rmse.test[j]  < graph.GB_RH.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_RH.male$pareto <- pareto_front(graph.GB_RH.male)

# Plot
Plot.EFF.GB.RH.male <- ggplot(graph.GB_RH.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_RH.male[graph.GB_RH.male$pareto, ][order(graph.GB_RH.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_RH.male[graph.GB_RH.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-RH Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.GB.RH.comb <- Plot.EFF.GB.RH.fem + Plot.EFF.GB.RH.male 
print(Plot.EFF.GB.RH.comb)
