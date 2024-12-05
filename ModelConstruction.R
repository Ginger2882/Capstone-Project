#define factors that will be used in model
factors <- c("eFG", "eFG.All.", "TOV_Perc.", "TOV_Perc.All.", 
             "ORB_Perc.", "DRB_Perc.", "FTFactor", "FTFactor.All.")

#create an empty list and set the starting model count
models <- list()
model_count <- 1

#loop through each combinations of factors of size 'i' and fit our logsitic regression models
for (i in 1:length(factors)) {
  comb <- combn(factors, i, simplify = FALSE) #get all combinations of size i
  for (j in 1:length(comb)) { #loop over each of our combinations
    formula <- as.formula(paste("cbind(Wins, Losses) ~", paste(comb[[j]], collapse = " + "))) #create formula
    model <- glm(formula, family = binomial(), data = four_factors) #fit the logistic regression model
    assign(paste0("model", model_count), model) #save the model with names with model_count
    cat("Created model", model_count, "with formula:", deparse(formula), "\n")  #Print model number and formula
    model_count <- model_count + 1  #increase model count
  }
}

#recreate models list and start new model count for 'since_warriors'
models <- list()
model_count <- 256

#repeat process for creating model for 'since_warriors'
for (i in 1:length(factors)) {
  comb <- combn(factors, i, simplify = FALSE)
  for (j in 1:length(comb)) {
    formula <- as.formula(paste("cbind(Wins, Losses) ~", paste(comb[[j]], collapse = " + ")))
    model <- glm(formula, family = binomial(), data = since_warriors)
    assign(paste0("model", model_count), model)
    cat("Created model", model_count, "with formula:", deparse(formula), "\n")
    model_count <- model_count + 1
  }
}

#recreate models list and start new model count for 'before_warriors'
models <- list()
model_count <- 511

#repeat process for creating model for 'before_warriors'
for (i in 1:length(factors)) {
  comb <- combn(factors, i, simplify = FALSE)
  for (j in 1:length(comb)) {
    formula <- as.formula(paste("cbind(Wins, Losses) ~", paste(comb[[j]], collapse = " + ")))
    model <- glm(formula, family = binomial(), data = before_warriors)
    assign(paste0("model", model_count), model)
    cat("Created model", model_count, "with formula:", deparse(formula), "\n")
    model_count <- model_count + 1
  }
}

#recreate models list and start new model count for 'since_defense'
models <- list()
model_count <- 766

#repeat process for creating model for 'since_defense'
for (i in 1:length(factors)) {
  comb <- combn(factors, i, simplify = FALSE)
  for (j in 1:length(comb)) {
    formula <- as.formula(paste("cbind(Wins, Losses) ~", paste(comb[[j]], collapse = " + ")))
    model <- glm(formula, family = binomial(), data = since_defense)
    assign(paste0("model", model_count), model)
    cat("Created model", model_count, "with formula:", deparse(formula), "\n")
    model_count <- model_count + 1
  }
}

#recreate models list and start new model count for 'before_defense'
models <- list()
model_count <- 1021

#repeat process for creating model for 'before_defense'
for (i in 1:length(factors)) {
  comb <- combn(factors, i, simplify = FALSE)
  for (j in 1:length(comb)) {
    formula <- as.formula(paste("cbind(Wins, Losses) ~", paste(comb[[j]], collapse = " + ")))
    model <- glm(formula, family = binomial(), data = before_defense)
    assign(paste0("model", model_count), model)
    cat("Created model", model_count, "with formula:", deparse(formula), "\n")
    model_count <- model_count + 1
  }
}

#view models
print(model255)
print(model510)
print(model765)
print(model1020)
print(model1275)
