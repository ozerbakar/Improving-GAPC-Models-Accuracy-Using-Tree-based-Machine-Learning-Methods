
Deaths.test.GB.LC <- Deaths.test
Deaths.test.GB.LC$psi.LC <- 0

# Creating learner
gb <- makeLearner("regr.gbm", predict.type = "response")

# Creating training and testing tasks
trainTask.GB.LC <- makeRegrTask(data = Deaths.LC, target = "psi.LC")
testTask.GB.LC <- makeRegrTask(data = Deaths.test.GB.LC, target = "psi.LC")

# Creating parameters grid
gb_params <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  n.trees = c(50, 100, 150),
  interaction.depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_GB.LC.train <- Deaths.LC
output_GB.LC.test <- Deaths.test.GB.LC

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
    task = trainTask.GB.LC,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.gb <- train(pars.gb, trainTask.GB.LC)
  
  # Prediction of the model
  preds.test.gb <- predict(model.gb, task = testTask.GB.LC)
  preds.train.gb <- predict(model.gb, task = trainTask.GB.LC)
  
  # Saving predictions
  output_GB.LC.test[[paste0("hp ",i)]] <- preds.test.gb$data$response
  output_GB.LC.train[[paste0("hp ",i)]] <- preds.train.gb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.LC <- as.vector(as.matrix(for.m.LC.fem))
Rate.test.male.LC <- as.vector(as.matrix(for.m.LC.male))

test.psi.fem.GB.LC <- output_GB.LC.test %>% filter(Gender == "f")
test.psi.fem.GB.LC <- test.psi.fem.GB.LC[-1:-5]
test.psi.male.GB.LC <- output_GB.LC.test %>% filter(Gender == "m")
test.psi.male.GB.LC <- test.psi.male.GB.LC[-1:-5]

test.imp.rates.fem.GB.LC <- as.data.frame(as.matrix(test.psi.fem.GB.LC)*Rate.test.fem.LC)
test.imp.rates.fem.GB.LC$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.GB.LC <- as.data.frame(as.matrix(test.psi.male.GB.LC)*Rate.test.male.LC)
test.imp.rates.male.GB.LC$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.GB.LC.fem <- c()
test.rmse_results.GB.LC.male <- c()

for (i in colnames(test.imp.rates.fem.GB.LC)) {
  if (i != "obs.test") {
    test.rmse_val.GB.LC.fem <- RMSE(test.imp.rates.fem.GB.LC[[i]], test.imp.rates.fem.GB.LC$obs.test)
    test.rmse_results.GB.LC.fem[i] <- test.rmse_val.GB.LC.fem
  }
}

for (i in colnames(test.imp.rates.male.GB.LC)) {
  if (i != "obs.test") {
    test.rmse_val.GB.LC.male <- RMSE(test.imp.rates.male.GB.LC[[i]], test.imp.rates.male.GB.LC$obs.test)
    test.rmse_results.GB.LC.male[i] <- test.rmse_val.GB.LC.male
  }
}

test.rmse.GB.LC.fem <- data.frame(
  Hyper.p = names(test.rmse_results.GB.LC.fem),
  rmse.test = as.numeric(test.rmse_results.GB.LC.fem)
)
test.rmse.GB.LC.male <- data.frame(
  Hyper.p = names(test.rmse_results.GB.LC.male),
  rmse.test = as.numeric(test.rmse_results.GB.LC.male)
)

test.rmse.lower.GB.LC.fem <- test.rmse.GB.LC.fem %>% filter(rmse.test < rmse.for.LC.fem)
test.rmse.lower.GB.LC.male <- test.rmse.GB.LC.male %>% filter(rmse.test < rmse.for.LC.male)

Rate.train.fem.LC <- as.vector(as.matrix(m.LC.fem))
Rate.train.male.LC <- as.vector(as.matrix(m.LC.male))

train.psi.fem.GB.LC <- output_GB.LC.train %>% filter(Gender == "f")
train.psi.fem.GB.LC <- train.psi.fem.GB.LC[-1:-5]
train.psi.male.GB.LC <- output_GB.LC.train %>% filter(Gender == "m")
train.psi.male.GB.LC <- train.psi.male.GB.LC[-1:-5]

train.imp.rates.fem.GB.LC <- as.data.frame(as.matrix(train.psi.fem.GB.LC)*Rate.train.fem.LC)
train.imp.rates.fem.GB.LC$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.GB.LC <- as.data.frame(as.matrix(train.psi.male.GB.LC)*Rate.train.male.LC)
train.imp.rates.male.GB.LC$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.GB.LC.fem <- c()
train.rmse_results.GB.LC.male <- c()

for (i in colnames(train.imp.rates.fem.GB.LC)) {
  if (i != "obs.train") {
    train.rmse_val.GB.LC.fem <- RMSE(train.imp.rates.fem.GB.LC[[i]], train.imp.rates.fem.GB.LC$obs.train)
    train.rmse_results.GB.LC.fem[i] <- train.rmse_val.GB.LC.fem
  }
}
for (i in colnames(train.imp.rates.male.GB.LC)) {
  if (i != "obs.train") {
    train.rmse_val.GB.LC.male <- RMSE(train.imp.rates.male.GB.LC[[i]], train.imp.rates.male.GB.LC$obs.train)
    train.rmse_results.GB.LC.male[i] <- train.rmse_val.GB.LC.male
  }
}
train.rmse.GB.LC.fem <- data.frame(
  Hyper.p = names(train.rmse_results.GB.LC.fem),
  rmse.train = as.numeric(train.rmse_results.GB.LC.fem))
train.rmse.GB.LC.male <- data.frame(
  Hyper.p = names(train.rmse_results.GB.LC.male),
  rmse.train = as.numeric(train.rmse_results.GB.LC.male))

train.rmse.lower.GB.LC.fem <- train.rmse.GB.LC.fem %>% filter(rmse.train < rmse.fit.LC.fem)
tt.rmse.GB.LC.fem <- inner_join(test.rmse.lower.GB.LC.fem, train.rmse.lower.GB.LC.fem, by = "Hyper.p")
train.rmse.lower.GB.LC.male <- train.rmse.GB.LC.male %>% filter(rmse.train < rmse.fit.LC.male)
tt.rmse.GB.LC.male <- inner_join(test.rmse.lower.GB.LC.male, train.rmse.lower.GB.LC.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.GB.LC.fem$rmse.test <- round(tt.rmse.GB.LC.fem$rmse.test, digits = 8)
tt.rmse.GB.LC.fem$rmse.train <- round(tt.rmse.GB.LC.fem$rmse.train, digits = 8)
tt.rmse.GB.LC.male$rmse.test <- round(tt.rmse.GB.LC.male$rmse.test, digits = 8)
tt.rmse.GB.LC.male$rmse.train <- round(tt.rmse.GB.LC.male$rmse.train, digits = 8)

graph.GB_LC.fem <- tt.rmse.GB.LC.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.GB_LC.male <- tt.rmse.GB.LC.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.GB_LC.fem <- graph.GB_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.GB_LC.male <- graph.GB_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.GB_LC.fem <- graph.GB_LC.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.GB_LC.male <- graph.GB_LC.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.GB_LC.fem <- graph.GB_LC.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.GB_LC.male <- graph.GB_LC.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.GB_LC.fem$Hyper.p <- factor(graph.GB_LC.fem$Hyper.p,
                                  levels = graph.GB_LC.fem$Hyper.p[order(as.numeric(str_extract(graph.GB_LC.fem$Hyper.p, "[0-9]+")))])
graph.GB_LC.male$Hyper.p <- factor(graph.GB_LC.male$Hyper.p,
                                    levels = graph.GB_LC.male$Hyper.p[order(as.numeric(str_extract(graph.GB_LC.male$Hyper.p, "[0-9]+")))])

plot.GB.LC.fem.test <- ggplot(graph.GB_LC.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.LC.fem.train <- ggplot(graph.GB_LC.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.LC.fem.comb <- plot.GB.LC.fem.train + plot.GB.LC.fem.test + 
  plot_annotation(title = "GB-LC Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.LC.fem.comb)

plot.GB.LC.male.test <- ggplot(graph.GB_LC.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.LC.male.train <- ggplot(graph.GB_LC.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.LC.male.comb <- plot.GB.LC.male.train + plot.GB.LC.male.test + 
  plot_annotation(title = "GB-LC Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.LC.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.GB_LC.fem) {
  keep <- rep(TRUE, nrow(graph.GB_LC.fem))
  for (i in 1:nrow(graph.GB_LC.fem)) {
    for (j in 1:nrow(graph.GB_LC.fem)) {
      if (i != j) {
        if (graph.GB_LC.fem$rmse.train[j] <= graph.GB_LC.fem$rmse.train[i] &&
            graph.GB_LC.fem$rmse.test[j]  <= graph.GB_LC.fem$rmse.test[i] &&
            (graph.GB_LC.fem$rmse.train[j] < graph.GB_LC.fem$rmse.train[i] ||
             graph.GB_LC.fem$rmse.test[j]  < graph.GB_LC.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_LC.fem$pareto <- pareto_front(graph.GB_LC.fem)

# Plot
Plot.EFF.GB.LC.fem <- ggplot(graph.GB_LC.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_LC.fem[graph.GB_LC.fem$pareto, ][order(graph.GB_LC.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_LC.fem[graph.GB_LC.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-LC Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.GB_LC.male) {
  keep <- rep(TRUE, nrow(graph.GB_LC.male))
  for (i in 1:nrow(graph.GB_LC.male)) {
    for (j in 1:nrow(graph.GB_LC.male)) {
      if (i != j) {
        if (graph.GB_LC.male$rmse.train[j] <= graph.GB_LC.male$rmse.train[i] &&
            graph.GB_LC.male$rmse.test[j]  <= graph.GB_LC.male$rmse.test[i] &&
            (graph.GB_LC.male$rmse.train[j] < graph.GB_LC.male$rmse.train[i] ||
             graph.GB_LC.male$rmse.test[j]  < graph.GB_LC.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_LC.male$pareto <- pareto_front(graph.GB_LC.male)

# Plot
Plot.EFF.GB.LC.male <- ggplot(graph.GB_LC.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_LC.male[graph.GB_LC.male$pareto, ][order(graph.GB_LC.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_LC.male[graph.GB_LC.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-LC Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.GB.LC.comb <- Plot.EFF.GB.LC.fem + Plot.EFF.GB.LC.male 
print(Plot.EFF.GB.LC.comb)
