
Deaths.test.GB.Plat <- Deaths.test
Deaths.test.GB.Plat$psi.Plat <- 0

# Creating learner
gb <- makeLearner("regr.gbm", predict.type = "response")

# Creating training and testing tasks
trainTask.GB.Plat <- makeRegrTask(data = Deaths.Plat, target = "psi.Plat")
testTask.GB.Plat <- makeRegrTask(data = Deaths.test.GB.Plat, target = "psi.Plat")

# Creating parameters grid
gb_params <- expand.grid(
  shrinkage = c(0.01, 0.05, 0.1),
  n.trees = c(50, 100, 150),
  interaction.depth = c(1, 3, 5)
)

# Setting cross-validation 
cv_fold <- makeResampleDesc("CV", iters = 5L)

# Create outputs for saving estimations
output_GB.Plat.train <- Deaths.Plat
output_GB.Plat.test <- Deaths.test.GB.Plat

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
    task = trainTask.GB.Plat,
    resampling = cv_fold,
    measures = mse
  )
  
  # Training the model
  model.gb <- train(pars.gb, trainTask.GB.Plat)
  
  # Prediction of the model
  preds.test.gb <- predict(model.gb, task = testTask.GB.Plat)
  preds.train.gb <- predict(model.gb, task = trainTask.GB.Plat)
  
  # Saving predictions
  output_GB.Plat.test[[paste0("hp ",i)]] <- preds.test.gb$data$response
  output_GB.Plat.train[[paste0("hp ",i)]] <- preds.train.gb$data$response
}

# Determining psi series that gives lower RMSE over training period 
# those already produced lower errors over testing period
Rate.test.fem.Plat <- as.vector(as.matrix(for.m.Plat.fem))
Rate.test.male.Plat <- as.vector(as.matrix(for.m.Plat.male))

test.psi.fem.GB.Plat <- output_GB.Plat.test %>% filter(Gender == "f")
test.psi.fem.GB.Plat <- test.psi.fem.GB.Plat[-1:-5]
test.psi.male.GB.Plat <- output_GB.Plat.test %>% filter(Gender == "m")
test.psi.male.GB.Plat <- test.psi.male.GB.Plat[-1:-5]

test.imp.rates.fem.GB.Plat <- as.data.frame(as.matrix(test.psi.fem.GB.Plat)*Rate.test.fem.Plat)
test.imp.rates.fem.GB.Plat$obs.test <- as.vector(as.matrix(m.obs.test.fem))

test.imp.rates.male.GB.Plat <- as.data.frame(as.matrix(test.psi.male.GB.Plat)*Rate.test.male.Plat)
test.imp.rates.male.GB.Plat$obs.test <- as.vector(as.matrix(m.obs.test.male))

test.rmse_results.GB.Plat.fem <- c()
test.rmse_results.GB.Plat.male <- c()

for (i in colnames(test.imp.rates.fem.GB.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.GB.Plat.fem <- RMSE(test.imp.rates.fem.GB.Plat[[i]], test.imp.rates.fem.GB.Plat$obs.test)
    test.rmse_results.GB.Plat.fem[i] <- test.rmse_val.GB.Plat.fem
  }
}

for (i in colnames(test.imp.rates.male.GB.Plat)) {
  if (i != "obs.test") {
    test.rmse_val.GB.Plat.male <- RMSE(test.imp.rates.male.GB.Plat[[i]], test.imp.rates.male.GB.Plat$obs.test)
    test.rmse_results.GB.Plat.male[i] <- test.rmse_val.GB.Plat.male
  }
}

test.rmse.GB.Plat.fem <- data.frame(
  Hyper.p = names(test.rmse_results.GB.Plat.fem),
  rmse.test = as.numeric(test.rmse_results.GB.Plat.fem)
)
test.rmse.GB.Plat.male <- data.frame(
  Hyper.p = names(test.rmse_results.GB.Plat.male),
  rmse.test = as.numeric(test.rmse_results.GB.Plat.male)
)

test.rmse.lower.GB.Plat.fem <- test.rmse.GB.Plat.fem %>% filter(rmse.test < rmse.for.Plat.fem)
test.rmse.lower.GB.Plat.male <- test.rmse.GB.Plat.male %>% filter(rmse.test < rmse.for.Plat.male)

Rate.train.fem.Plat <- as.vector(as.matrix(m.Plat.fem))
Rate.train.male.Plat <- as.vector(as.matrix(m.Plat.male))

train.psi.fem.GB.Plat <- output_GB.Plat.train %>% filter(Gender == "f")
train.psi.fem.GB.Plat <- train.psi.fem.GB.Plat[-1:-5]
train.psi.male.GB.Plat <- output_GB.Plat.train %>% filter(Gender == "m")
train.psi.male.GB.Plat <- train.psi.male.GB.Plat[-1:-5]

train.imp.rates.fem.GB.Plat <- as.data.frame(as.matrix(train.psi.fem.GB.Plat)*Rate.train.fem.Plat)
train.imp.rates.fem.GB.Plat$obs.train <- as.vector(as.matrix(m.obs.train.fem))
train.imp.rates.male.GB.Plat <- as.data.frame(as.matrix(train.psi.male.GB.Plat)*Rate.train.male.Plat)
train.imp.rates.male.GB.Plat$obs.train <- as.vector(as.matrix(m.obs.train.male))

train.rmse_results.GB.Plat.fem <- c()
train.rmse_results.GB.Plat.male <- c()

for (i in colnames(train.imp.rates.fem.GB.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.GB.Plat.fem <- RMSE(train.imp.rates.fem.GB.Plat[[i]], train.imp.rates.fem.GB.Plat$obs.train)
    train.rmse_results.GB.Plat.fem[i] <- train.rmse_val.GB.Plat.fem
  }
}
for (i in colnames(train.imp.rates.male.GB.Plat)) {
  if (i != "obs.train") {
    train.rmse_val.GB.Plat.male <- RMSE(train.imp.rates.male.GB.Plat[[i]], train.imp.rates.male.GB.Plat$obs.train)
    train.rmse_results.GB.Plat.male[i] <- train.rmse_val.GB.Plat.male
  }
}
train.rmse.GB.Plat.fem <- data.frame(
  Hyper.p = names(train.rmse_results.GB.Plat.fem),
  rmse.train = as.numeric(train.rmse_results.GB.Plat.fem))
train.rmse.GB.Plat.male <- data.frame(
  Hyper.p = names(train.rmse_results.GB.Plat.male),
  rmse.train = as.numeric(train.rmse_results.GB.Plat.male))

train.rmse.lower.GB.Plat.fem <- train.rmse.GB.Plat.fem %>% filter(rmse.train < rmse.fit.Plat.fem)
tt.rmse.GB.Plat.fem <- inner_join(test.rmse.lower.GB.Plat.fem, train.rmse.lower.GB.Plat.fem, by = "Hyper.p")
train.rmse.lower.GB.Plat.male <- train.rmse.GB.Plat.male %>% filter(rmse.train < rmse.fit.Plat.male)
tt.rmse.GB.Plat.male <- inner_join(test.rmse.lower.GB.Plat.male, train.rmse.lower.GB.Plat.male, by = "Hyper.p")

# Graphical Representation
tt.rmse.GB.Plat.fem$rmse.test <- round(tt.rmse.GB.Plat.fem$rmse.test, digits = 8)
tt.rmse.GB.Plat.fem$rmse.train <- round(tt.rmse.GB.Plat.fem$rmse.train, digits = 8)
tt.rmse.GB.Plat.male$rmse.test <- round(tt.rmse.GB.Plat.male$rmse.test, digits = 8)
tt.rmse.GB.Plat.male$rmse.train <- round(tt.rmse.GB.Plat.male$rmse.train, digits = 8)

graph.GB_Plat.fem <- tt.rmse.GB.Plat.fem %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)
graph.GB_Plat.male <- tt.rmse.GB.Plat.male %>%
  distinct(rmse.train, rmse.test, .keep_all = TRUE)

min.test.GB_Plat.fem <- graph.GB_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.test.GB_Plat.male <- graph.GB_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  distinct(rmse.test)
min.train.GB_Plat.fem <- graph.GB_Plat.fem %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.train.GB_Plat.male <- graph.GB_Plat.male %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

min.test.train.GB_Plat.fem <- graph.GB_Plat.fem %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)
min.test.train.GB_Plat.male <- graph.GB_Plat.male %>%
  filter(rmse.test == min(rmse.test)) %>%
  filter(rmse.train == min(rmse.train)) %>%
  distinct(rmse.train)

graph.GB_Plat.fem$Hyper.p <- factor(graph.GB_Plat.fem$Hyper.p,
                                   levels = graph.GB_Plat.fem$Hyper.p[order(as.numeric(str_extract(graph.GB_Plat.fem$Hyper.p, "[0-9]+")))])
graph.GB_Plat.male$Hyper.p <- factor(graph.GB_Plat.male$Hyper.p,
                                    levels = graph.GB_Plat.male$Hyper.p[order(as.numeric(str_extract(graph.GB_Plat.male$Hyper.p, "[0-9]+")))])

plot.GB.Plat.fem.test <- ggplot(graph.GB_Plat.fem, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.Plat.fem.train <- ggplot(graph.GB_Plat.fem, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.Plat.fem.comb <- plot.GB.Plat.fem.train + plot.GB.Plat.fem.test + 
  plot_annotation(title = "GB-Plat Female",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.Plat.fem.comb)

plot.GB.Plat.male.test <- ggplot(graph.GB_Plat.male, aes(x = Hyper.p, y = rmse.train)) +
  geom_point(color = "blue", size = 2) +
  labs(
    title = "TRAIN",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

plot.GB.Plat.male.train <- ggplot(graph.GB_Plat.male, aes(x = Hyper.p, y = rmse.test)) +
  geom_point(color = "red", size = 2) +
  labs(
    title = "TEST",
    x = "Hyperparameter Set",
    y = ""
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3),
        plot.title = element_text(hjust = 0.5))

Plot.GB.Plat.male.comb <- plot.GB.Plat.male.train + plot.GB.Plat.male.test + 
  plot_annotation(title = "GB-Plat Male",
                  theme = theme(plot.title = element_text(hjust = 0.5, face = "bold")))

print(Plot.GB.Plat.male.comb)

# Graph for Efficient Frontier
# Pareto optimal function
pareto_front <- function(graph.GB_Plat.fem) {
  keep <- rep(TRUE, nrow(graph.GB_Plat.fem))
  for (i in 1:nrow(graph.GB_Plat.fem)) {
    for (j in 1:nrow(graph.GB_Plat.fem)) {
      if (i != j) {
        if (graph.GB_Plat.fem$rmse.train[j] <= graph.GB_Plat.fem$rmse.train[i] &&
            graph.GB_Plat.fem$rmse.test[j]  <= graph.GB_Plat.fem$rmse.test[i] &&
            (graph.GB_Plat.fem$rmse.train[j] < graph.GB_Plat.fem$rmse.train[i] ||
             graph.GB_Plat.fem$rmse.test[j]  < graph.GB_Plat.fem$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_Plat.fem$pareto <- pareto_front(graph.GB_Plat.fem)

# Plot
Plot.EFF.GB.Plat.fem <- ggplot(graph.GB_Plat.fem, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_Plat.fem[graph.GB_Plat.fem$pareto, ][order(graph.GB_Plat.fem$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_Plat.fem[graph.GB_Plat.fem$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-Plat Female",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

# Pareto optimal function
pareto_front <- function(graph.GB_Plat.male) {
  keep <- rep(TRUE, nrow(graph.GB_Plat.male))
  for (i in 1:nrow(graph.GB_Plat.male)) {
    for (j in 1:nrow(graph.GB_Plat.male)) {
      if (i != j) {
        if (graph.GB_Plat.male$rmse.train[j] <= graph.GB_Plat.male$rmse.train[i] &&
            graph.GB_Plat.male$rmse.test[j]  <= graph.GB_Plat.male$rmse.test[i] &&
            (graph.GB_Plat.male$rmse.train[j] < graph.GB_Plat.male$rmse.train[i] ||
             graph.GB_Plat.male$rmse.test[j]  < graph.GB_Plat.male$rmse.test[i])) {
          keep[i] <- FALSE
          break
        }
      }
    }
  }
  return(keep)
}
graph.GB_Plat.male$pareto <- pareto_front(graph.GB_Plat.male)

# Plot
Plot.EFF.GB.Plat.male <- ggplot(graph.GB_Plat.male, aes(x = rmse.train, y = rmse.test, color = pareto)) +
  geom_point(size = 4) +
  geom_line(data = graph.GB_Plat.male[graph.GB_Plat.male$pareto, ][order(graph.GB_Plat.male$rmse.train), ], 
            aes(x = rmse.train, y = rmse.test), 
            color = "red") +
  geom_text(data = graph.GB_Plat.male[graph.GB_Plat.male$pareto, ], aes(label = Hyper.p), vjust = -1, hjust = 0.5, color = "black", size = 3) +
  scale_color_manual(values = c("darkgreen", "red")) +
  labs(title = "GB-Plat Male",
       x = "Train", 
       y = "Test") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        axis.title.x = element_text(face = "bold", size = 15),
        axis.title.y = element_text(face = "bold", size = 15))

Plot.EFF.GB.Plat.comb <- Plot.EFF.GB.Plat.fem + Plot.EFF.GB.Plat.male 
print(Plot.EFF.GB.Plat.comb)
