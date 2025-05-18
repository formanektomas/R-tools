# An infinite number of aliens take turns slicing and taking a random 
# amount of planet Earth (each subsequent alien selects a random 
# portion from what remains). What’s the probability that the first 
# alien got the largest slice?


# Set the number of repetitions and individuals
repetitions <- 10000000
individuals <- 20

# Function to simulate the slicing process
simulate_slicing <- function(individuals) {
  remaining <- 1
  slices <- numeric(individuals)
  
  for (i in 1:individuals) {
    slice <- runif(1, 0,1)
    slices[i] <- remaining * slice
    remaining <- remaining - slices[i]
  }
  
  return(slices)
}

# Run the simulation
results <- replicate(repetitions, simulate_slicing(individuals))

# Calculate the probability that the first alien got the largest slice
largest_slice_first <- sum(apply(results, 2, function(x) which.max(x) == 1)) / repetitions

# Print the result
cat("Probability that the first alien got the largest slice:", largest_slice_first, "\n")
#

x <- t(results)
x <- as.data.frame(x)
x$row_sum <- rowSums(x[, sapply(x, is.numeric)])
summary(x)


library(ggplot2)
library(tidyr)
library(dplyr)

# Reshape to long format
x_long <- pivot_longer(x[,1:3], cols = everything(), names_to = "Variable", values_to = "Value")

# Plot density
ggplot(x_long, aes(x = Value, fill = Variable)) +
  geom_density(alpha = 0.5, bw = 0.001) +
  labs(x = "Value", y = "Density") +
  theme_minimal()

