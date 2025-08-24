
Deaths.test.GB.APC <- Deaths.test
Deaths.test.GB.APC$psi.APC <- 0

# Creating learner
gb <- makeLearner("regr.gbm", predict.type = "response")

# Creating training and testing tasks
trainTask.GB.APC <- makeRegrTask(data = Deaths.APC, target = "psi.APC")
testTask.GB.APC <- makeRegrTask(data = Deaths.test.GB.APC, target = "psi.APC")

# Creating parameters grid
gb_params <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  n.trees = c(50, 100, 150),
  interaction.depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_GB.APC.train <- Deaths.APC
output_GB.APC.test <- Deaths.test.GB.APC

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
    task = trainTask.GB.APC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.gb <- train(pars.gb, trainTask.GB.APC)
  
  # Prediction of the model
  preds.test.gb <- predict(model.gb, task = testTask.GB.APC)
  preds.train.gb <- predict(model.gb, task = trainTask.GB.APC)
  
  # Saving predictions
  output_GB.APC.test[[paste0("hp ",i)]] <- preds.test.gb$data$response
  output_GB.APC.train[[paste0("hp ",i)]] <- preds.train.gb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.APC <- as.vector(as.matrix(for.m.APC.fem))
Rate.test.male.APC <- as.vector(as.matrix(for.m.APC.male))

test.psi.fem.GB.APC <- output_GB.APC.test %>% filter(Gender == "f")
test.psi.fem.GB.APC <- test.psi.fem.GB.APC[-1:-5]
test.psi.male.GB.APC <- output_GB.APC.test %>% filter(Gender == "m")
test.psi.male.GB.APC <- test.psi.male.GB.APC[-1:-5]

test.imp.rates.fem.GB.APC <- as.data.frame(as.matrix(test.psi.fem.GB.APC)*Rate.test.fem.APC)
test.imp.rates.fem.GB.APC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.GB.APC <- as.data.frame(as.matrix(test.psi.male.GB.APC)*Rate.test.male.APC)
test.imp.rates.male.GB.APC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.GB.APC.fem <- c()
test.rmse_results.GB.APC.male <- c()

for (i in colnames(test.imp.rates.fem.GB.APC)) {
  if (i != "obs.test") {
    test.rmse_val.GB.APC.fem <- RMSE(test.imp.rates.fem.GB.APC[[i]], test.imp.rates.fem.GB.APC$obs.test)
    test.rmse_results.GB.APC.fem[i] <- test.rmse_val.GB.APC.fem
  }
}

for (i in colnames(test.imp.rates.male.GB.APC)) {
  if (i != "obs.test") {
    test.rmse_val.GB.APC.male <- RMSE(test.imp.rates.male.GB.APC[[i]], test.imp.rates.male.GB.APC$obs.test)
    test.rmse_results.GB.APC.male[i] <- test.rmse_val.GB.APC.male
  }
}

test.rmse.GB.APC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.GB.APC.fem),
  rmse.test = as.numeric(test.rmse_results.GB.APC.fem)
)
test.rmse.GB.APC.male <- data.frame(
  Hyper.p = names(test.rmse_results.GB.APC.male),
  rmse.test = as.numeric(test.rmse_results.GB.APC.male)
)

test.rmse.lower.GB.APC.fem <- test.rmse.GB.APC.fem %>% filter(rmse.test < rmse.for.APC.fem)
test.rmse.lower.GB.APC.male <- test.rmse.GB.APC.male %>% filter(rmse.test < rmse.for.APC.male)

Rate.train.fem.APC <- as.vector(as.matrix(m.APC.fem))
Rate.train.male.APC <- as.vector(as.matrix(m.APC.male))

train.psi.fem.GB.APC <- output_GB.APC.train %>% filter(Gender == "f")
train.psi.fem.GB.APC <- train.psi.fem.GB.APC[-1:-5]
train.psi.male.GB.APC <- output_GB.APC.train %>% filter(Gender == "m")
train.psi.male.GB.APC <- train.psi.male.GB.APC[-1:-5]

train.imp.rates.fem.GB.APC <- as.data.frame(as.matrix(train.psi.fem.GB.APC)*Rate.train.fem.APC)
train.imp.rates.fem.GB.APC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.GB.APC <- as.data.frame(as.matrix(train.psi.male.GB.APC)*Rate.train.male.APC)
train.imp.rates.male.GB.APC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.GB.APC.fem <- c()
train.rmse_results.GB.APC.male <- c()

for (i in colnames(train.imp.rates.fem.GB.APC)) {
  if (i != "obs.train") {
    train.rmse_val.GB.APC.fem <- RMSE(train.imp.rates.fem.GB.APC[[i]], train.imp.rates.fem.GB.APC$obs.train)
    train.rmse_results.GB.APC.fem[i] <- train.rmse_val.GB.APC.fem
  }
}
for (i in colnames(train.imp.rates.male.GB.APC)) {
  if (i != "obs.train") {
    train.rmse_val.GB.APC.male <- RMSE(train.imp.rates.male.GB.APC[[i]], train.imp.rates.male.GB.APC$obs.train)
    train.rmse_results.GB.APC.male[i] <- train.rmse_val.GB.APC.male
  }
}
train.rmse.GB.APC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.GB.APC.fem),
  rmse.train = as.numeric(train.rmse_results.GB.APC.fem))
train.rmse.GB.APC.male <- data.frame(
  Hyper.p = names(train.rmse_results.GB.APC.male),
  rmse.train = as.numeric(train.rmse_results.GB.APC.male))

train.rmse.lower.GB.APC.fem <- train.rmse.GB.APC.fem %>% filter(rmse.train < rmse.fit.APC.fem)
tt.rmse.GB.APC.fem <- inner_join(test.rmse.lower.GB.APC.fem, train.rmse.lower.GB.APC.fem, by = "Hyper.p")
train.rmse.lower.GB.APC.male <- train.rmse.GB.APC.male %>% filter(rmse.train < rmse.fit.APC.male)
tt.rmse.GB.APC.male <- inner_join(test.rmse.lower.GB.APC.male, train.rmse.lower.GB.APC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.GB.APC.fem$rmse.test <- round(tt.rmse.GB.APC.fem$rmse.test, digits = 8)
tt.rmse.GB.APC.fem$rmse.train <- round(tt.rmse.GB.APC.fem$rmse.train, digits = 8)
tt.rmse.GB.APC.male$rmse.test <- round(tt.rmse.GB.APC.male$rmse.test, digits = 8)
tt.rmse.GB.APC.male$rmse.train <- round(tt.rmse.GB.APC.male$rmse.train, digits = 8)

graph.GB_APC.fem <- tt.rmse.GB.APC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.GB_APC.male <- tt.rmse.GB.APC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.GB_APC.fem <- graph.GB_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.GB_APC.male <- graph.GB_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.GB_APC.fem <- graph.GB_APC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.GB_APC.male <- graph.GB_APC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.GB_APC.fem <- graph.GB_APC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.GB_APC.male <- graph.GB_APC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.GB_APC.fem$Hyper.p <- factor(graph.GB_APC.fem$Hyper.p,
                                   levels = graph.GB_APC.fem$Hyper.p[order(as.numeric(str_extract(graph.GB_APC.fem$Hyper.p, "[0-9]+")))])
graph.GB_APC.male$Hyper.p <- factor(graph.GB_APC.male$Hyper.p,
                                    levels = graph.GB_APC.male$Hyper.p[order(as.numeric(str_extract(graph.GB_APC.male$Hyper.p, "[0-9]+")))])

plot.GB.APC.fem.test <- ggplot(graph.GB_APC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.APC.fem.train <- ggplot(graph.GB_APC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.APC.fem.comb <- plot.GB.APC.fem.train + plot.GB.APC.fem.test + 
  plot_annotation(title = "GB-APC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.APC.fem.comb)

plot.GB.APC.male.test <- ggplot(graph.GB_APC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.APC.male.train <- ggplot(graph.GB_APC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.APC.male.comb <- plot.GB.APC.male.train + plot.GB.APC.male.test + 
  plot_annotation(title = "GB-APC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.APC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.GB_APC.fem) {
  keep <- rep(TRUE, nrow(graph.GB_APC.fem))
  for (i in 1:nrow(graph.GB_APC.fem)) {
    for (j in 1:nrow(graph.GB_APC.fem)) {
      if (i != j) {
        if (graph.GB_APC.fem$rmse.train[j] <= graph.GB_APC.fem$rmse.train[i] &&
            graph.GB_APC.fem$rmse.test[j]  <= graph.GB_APC.fem$rmse.test[i] &&
            (graph.GB_APC.fem$rmse.train[j] < graph.GB_APC.fem$rmse.train[i] ||
             graph.GB_APC.fem$rmse.test[j]  < graph.GB_APC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_APC.fem$pareto <- pareto_front(graph.GB_APC.fem)

# Plot
Plot.EFF.GB.APC.fem <- ggplot(graph.GB_APC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_APC.fem[graph.GB_APC.fem$pareto, ][order(graph.GB_APC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_APC.fem[graph.GB_APC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-APC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.GB_APC.male) {
  keep <- rep(TRUE, nrow(graph.GB_APC.male))
  for (i in 1:nrow(graph.GB_APC.male)) {
    for (j in 1:nrow(graph.GB_APC.male)) {
      if (i != j) {
        if (graph.GB_APC.male$rmse.train[j] <= graph.GB_APC.male$rmse.train[i] &&
            graph.GB_APC.male$rmse.test[j]  <= graph.GB_APC.male$rmse.test[i] &&
            (graph.GB_APC.male$rmse.train[j] < graph.GB_APC.male$rmse.train[i] ||
             graph.GB_APC.male$rmse.test[j]  < graph.GB_APC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_APC.male$pareto <- pareto_front(graph.GB_APC.male)

# Plot
Plot.EFF.GB.APC.male <- ggplot(graph.GB_APC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_APC.male[graph.GB_APC.male$pareto, ][order(graph.GB_APC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_APC.male[graph.GB_APC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-APC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.GB.APC.comb <- Plot.EFF.GB.APC.fem + Plot.EFF.GB.APC.male 
print(Plot.EFF.GB.APC.comb)
