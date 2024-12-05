#set percentage threshold for AIC and BIC selection
percentage_threshold <- 0.10

#create a vector of the model names in four_factors
model_names <- paste0("model", 1:255)

#retrieve models from environment
models <- mget(model_names, envir = .GlobalEnv)
models <- models[!sapply(models, function(x) is.null(x) || !inherits(x, "glm"))]

#create a data frame with AIC and BIC values 
four_factors_results <- data.frame(
  Model = names(models),
  AIC = sapply(models, function(m) tryCatch(AIC(m))),
  BIC = sapply(models, function(m) tryCatch(BIC(m)))
)

#view four_factors AIC & BIC
print(four_factors_results)

#define minimum aic and bic for four_factors
min_aic <- min(four_factors_results$AIC)
min_bic <- min(four_factors_results$BIC)

#create threshold for aic and bic based on percentage threshold and minimums
aic_threshold <- min_aic * (1 + percentage_threshold)
bic_threshold <- min_bic * (1 + percentage_threshold)

#select models that meet our threshold
selected_models <- four_factors_results[
  four_factors_results$AIC <= aic_threshold & four_factors_results$BIC <= bic_threshold, 
]

#create a vector of the model names in since_warriors
model_names2 <- paste0("model", 256:510)

#retrieve models from environment
models2 <- mget(model_names2, envir = .GlobalEnv)
models2 <- models2[!sapply(models2, function(x) is.null(x) || !inherits(x, "glm"))]

#create a data frame with AIC and BIC values 
since_warriors_results <- data.frame(
  Model = names(models2),
  AIC = sapply(models2, function(m) tryCatch(AIC(m))),
  BIC = sapply(models2, function(m) tryCatch(BIC(m)))
)

#view since_warriors AIC & BIC
print(since_warriors_results)

#define minimum aic and bic for since_warriors
min_aic2 <- min(since_warriors_results$AIC, na.rm = TRUE)
min_bic2 <- min(since_warriors_results$BIC, na.rm = TRUE)

#create threshold for aic and bic based on percentage threshold and minimums
aic_threshold2 <- min_aic2 * (1 + percentage_threshold)
bic_threshold2 <- min_bic2 * (1 + percentage_threshold)

#select models that meet our threshold
selected_models2 <- since_warriors_results[
  since_warriors_results$AIC <= aic_threshold2 & since_warriors_results$BIC <= bic_threshold2, 
]

#view since_warriors AIC & BIC
print(selected_models2)

#create a vector of the model names in before_warriors
model_names3 <- paste0("model", 511:765)

#retrieve models from environment
models3 <- mget(model_names3, envir = .GlobalEnv)
models3 <- models3[!sapply(models3, function(x) is.null(x) || !inherits(x, "glm"))]

#create a data frame with AIC and BIC values 
before_warriors_results <- data.frame(
  Model = names(models3),
  AIC = sapply(models3, function(m) tryCatch(AIC(m))),
  BIC = sapply(models3, function(m) tryCatch(BIC(m)))
)

#view AIC & BIC for before_warriors models
print(before_warriors_results)

#define minimum aic and bic for before_warriors
min_aic3 <- min(before_warriors_results$AIC, na.rm = TRUE)
min_bic3 <- min(before_warriors_results$BIC, na.rm = TRUE)

#create threshold for aic and bic based on percentage threshold and minimums
aic_threshold3 <- min_aic3 * (1 + percentage_threshold)
bic_threshold3 <- min_bic3 * (1 + percentage_threshold)

#select models that meet our threshold
selected_models3 <- before_warriors_results[
  before_warriors_results$AIC <= aic_threshold3 & before_warriors_results$BIC <= bic_threshold3, 
]

#create a vector of the model names in since_defense
model_names4 <- paste0("model", 766:1020)

#retrieve models from environment
models4 <- mget(model_names4, envir = .GlobalEnv)
models4 <- models4[!sapply(models4, function(x) is.null(x) || !inherits(x, "glm"))]

#create a data frame with AIC and BIC values
since_defense_results <- data.frame(
  Model = names(models4),
  AIC = sapply(models4, function(m) tryCatch(AIC(m))),
  BIC = sapply(models4, function(m) tryCatch(BIC(m)))
)

#view since_defense AIC & BIC
print(since_defense_results)

#define minimum aic and bic for since_defense
min_aic4 <- min(since_defense_results$AIC, na.rm = TRUE)
min_bic4 <- min(since_defense_results$BIC, na.rm = TRUE)

#create threshold for aic and bic based on percentage threshold and minimums
aic_threshold4 <- min_aic4 * (1 + percentage_threshold)
bic_threshold4 <- min_bic4 * (1 + percentage_threshold)

#select models that meet our threshold
selected_models4 <- since_defense_results[
  since_defense_results$AIC <= aic_threshold4 & since_defense_results$BIC <= bic_threshold4, 
]

#create a vector of the model names in before_defense
model_names5 <- paste0("model", 1021:1275)

#retrieve models from environment
models5 <- mget(model_names5, envir = .GlobalEnv)
models5 <- models5[!sapply(models5, function(x) is.null(x) || !inherits(x, "glm"))]

#create a data frame with AIC and BIC values
before_defense_results <- data.frame(
  Model = names(models5),
  AIC = sapply(models5, function(m) tryCatch(AIC(m))),
  BIC = sapply(models5, function(m) tryCatch(BIC(m)))
)

#view before_defense AIC & BIC
print(before_defense_results)

#define minimum aic and bic for before_defense
min_aic5 <- min(before_defense_results$AIC, na.rm = TRUE)
min_bic5 <- min(before_defense_results$BIC, na.rm = TRUE)

#create threshold for aic and bic based on percentage threshold and minimums
aic_threshold5 <- min_aic5 * (1 + percentage_threshold)
bic_threshold5 <- min_bic5 * (1 + percentage_threshold)

#select models that meet our threshold
selected_models5 <- before_defense_results[
  before_defense_results$AIC <= aic_threshold5 & before_defense_results$BIC <= bic_threshold5, 
]

#view selected models
print(selected_models)  #four_factors models
print(selected_models2) #since_warriors models
print(selected_models3) #before_warriors models
print(selected_models4) #since_defense models
print(selected_models5) #before_defense models

#view ANOVA for best models
anova(model255)   #four_factors ANOVA
anova(model510)   #since_warriors ANOVA
anova(model765)   #before_warriors ANOVA
anova(model1020)  #since_defense ANOVA
anova(model1275)  #before_defense ANOVA

#view best models
summary(model255)
summary(model510)
summary(model765)
summary(model1020)
summary(model1275)
