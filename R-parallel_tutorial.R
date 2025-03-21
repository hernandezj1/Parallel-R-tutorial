library(parallel)
data("iris")

# One regression

singleregression_time<- system.time({
  formula <- as.formula(paste("Sepal.Length ~ Sepal.Width"))
  model <- lm(Sepal.Length ~ Sepal.Width, data = iris)
})
summary(model)
print(singleregression_time)

# pay attention to the difference between user, system and complete time

# All possible regressions


numeric_columns <- colnames(iris)[1:4] # We identify the columns we are going to use

# Create an empty list to store all the model formulas after we have made them
allModelsList <- list()

# Loop through each column as dependent variable and use the rest to do 1,2,and 3 prediction combos
for (dependent_var in numeric_columns) {
  
  # Set the predictors to be all variables except the dependent variable using the setdiff formula
  predictors <- setdiff(numeric_columns, dependent_var)
  
  # we use lapply to run the combination function to generate the 1,2,3  combinations which are then stored in the variable combs
  combs <- unlist(lapply(1:3, function(i) combn(predictors, i, simplify = FALSE)), recursive = FALSE)
  
  # Load each combinatio as the formula and paste it in the correct format 
  for (comb in combs) {
    model_formula <- as.formula(paste(c(paste(dependent_var, "~ 1"), comb), collapse = " + "))
    
    # Append the formula to the list
    allModelsList[[paste(dependent_var, paste(comb, collapse = "_"), sep = "_")]] <- model_formula
  }
}

# Display every single one, for the iris dataset this should be 28 formulas 
allModelsList

#Now we will run all models sequentially and then parallely

# create a function that will run our models
                         
run_regression <- function(formula) {
  model <- lm(formula, data = iris)  # Run linear model
  return(summary(model))  # Return summary of the model
}
                         
# Sequential run

all_regressions_time_linear<- system.time({
  regression_results <- lapply(allModelsList, run_regression)
})
print(all_regressions_time_linear)

# Parallel run

# Set the number of cores to use buut first check how many you have. almost ALWAYS leave one open
                         
num_cores <- detectCores()  
cores_to_be_used<- 2


all_regressions_time_parallel<- system.time({
  regression_results <- mclapply(allModelsList, run_regression, mc.cores = cores_to_be_used)
})
                         
print(all_regressions_time_parallel)
# Display the regression results
regression_results




