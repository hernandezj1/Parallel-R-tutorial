library(parallel)
data("iris")

# one regression

singleregression_time<- system.time({
  formula <- as.formula(paste("Sepal.Length ~ Sepal.Width"))
  model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
})
summary(model)
print(singleregression_time)

# more regressions

variables <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Loop over all pairs of variables for 2-variable linear regression
all_pair_regressions_time<- system.time({
  for (i in 1:(length(variables)-1)) {
  
    for (j in (i+1):length(variables)) {
    
        formula <- as.formula(paste(variables[i], "~", variables[j]))
        model <- lm(formula, data = iris)
        model_name <- paste("model_", variables[i], "_", variables[j], sep = "")
        assign(model_name, model)
    }
  }
})

print(all_pair_regressions_time)

# All possible regressions


numeric_columns <- colnames(iris)[1:4]

# Create an empty list to store all the models
allModelsList <- list()

# Loop through each numeric column and create a regression formula
for (dependent_var in numeric_columns) {
  
  # Set the predictors to be all variables except the dependent variable
  predictors <- setdiff(numeric_columns, dependent_var)
  
  # Create a matrix of all possible combinations of predictors (1, 2, or 3 predictors)
  combs <- unlist(lapply(1:3, function(i) combn(predictors, i, simplify = FALSE)), recursive = FALSE)
  
  # Construct the formulas for each combination of predictors
  for (comb in combs) {
    model_formula <- as.formula(paste(c(paste(dependent_var, "~ 1"), comb), collapse = " + "))
    
    # Append the formula to the list
    allModelsList[[paste(dependent_var, paste(comb, collapse = "_"), sep = "_")]] <- model_formula
  }
}

# Display the constructed formulas
allModelsList

# Running it 

run_regression <- function(formula) {
  model <- lm(formula, data = iris)  # Run linear model
  return(summary(model))  # Return summary of the model
}

# Run all models in parallel using mclapply
# Set the number of cores to use.
num_cores <- detectCores()  
cores_to_be_used<- 4


all_regressions_time_parallel<- system.time({
  regression_results <- mclapply(allModelsList, run_regression, mc.cores = 2)
})
print(all_regressions_time_parallel)
# Display the regression results
regression_results


# Run all regressions linearly
all_regressions_time_linear<- system.time({
  regression_results_linear <- lapply(allModelsList, run_regression)
})
print(all_regressions_time_linear)


