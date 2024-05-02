## TESTING NASH EQUILIBRIUM SIMULATIONS OF DOSAGE-SENSITIVE & -INSENSITIVE
## GENE NETWORKS IN VARYING LEVELS OF STOICHOIMETRIC BALANCE
## AUTHOR: EMILY L. CASANOVA, PHD, ASST. PROF., LOYOLA UNIVERSITY, NEW ORLEANS
## PRODUCED FOR PAPER: "The Molecular Basis of Punctuated Equilibria: The Roles 
## of Developmental Genes in Stasis and Speciation"

#Load necessary libraries:
library(ggplot2)

## SIMULATION #1 - Testing nasheek DOSAGE-SENSITIVE simulation (epsilon = 0) with 10,000 
## iterations
# Define the deviation function
deviation <- function(cX, cY, cZ) {
  abs(cX - cY) + abs(cX - cZ) + abs(cY - cZ)
}
# Define the epsilon range
epsilon_lower <- 0.0
epsilon_upper <- 0.0
# Generate simulated concentrations
# Generate a random seed based on the current time and print it
random_seed <- as.integer(Sys.time())
print(random_seed)
set.seed(random_seed)
cX_values <- runif(10000, min = 0.1, max = 1.0)
cY_values <- runif(10000, min = 0.1, max = 1.0)
cZ_values <- runif(10000, min = 0.1, max = 1.0)
# Calculate deviations
deviations <- mapply(deviation, cX_values, cY_values, cZ_values)
# Check if deviations fall within the epsilon range
within_epsilon <- deviations > epsilon_upper  # This checks if the deviation exceeds the upper bound
# Counts number of deviations within epsilon range and prints to screen
within_epsilon <- deviations >= epsilon_lower & deviations <= epsilon_upper
count_within_epsilon <- sum(within_epsilon)
print(paste("Number of instances where epsilon = 0:", count_within_epsilon))
# The above code results in 0 instances in which epsilon = 0 after 10,000 iterations
_______________________

## SIMULATION #2 - Testing nasheek DOSAGE-INSENSITIVE simulation (range = 0-0.3 per gene, 
## epsilon upper bound = 0.3 x 3 = 0.9):
# Define the deviation function
deviationDI <- function(cX, cY, cZ) {
  abs(cX - cY) + abs(cX - cZ) + abs(cY - cZ)
}
# Define the epsilon range
epsilon_lowerDS <- 0.0
epsilon_upperDS <- 0.9
# Generate simulated concentrations
# Generate a random seed based on the current time and print it
random_seedDI <- as.integer(Sys.time())
print(random_seedDI)
set.seed(random_seedDI)
cX_valuesDI <- runif(1000, min = 0.1, max = 1.0)
cY_valuesDI <- runif(1000, min = 0.1, max = 1.0)
cZ_valuesDI <- runif(1000, min = 0.1, max = 1.0)
# Calculate deviations
deviationsDI <- mapply(deviationDI, cX_valuesDI, cY_valuesDI, cZ_valuesDI)
# Check if deviations fall within the epsilon range
within_epsilonDI <- deviationsDI > epsilon_upperDI  # This checks if the deviation exceeds the upper bound
# Counts number of deviations within epsilon range and prints to screen
within_epsilonDI <- deviationsDI >= epsilon_lowerDI & deviationsDI <= epsilon_upperDI
count_within_epsilonDI <- sum(within_epsilonDI)
print(paste("Number of instances within the epsilon range:", count_within_epsilonDI))
# Create a data frame for plotting with the corrected condition
dataDI <- data.frame(Iteration = 1:1000, DeviationDI = deviationsDI, ExceedsUpperBoundDI = within_epsilonDI)
# Plot the results with color coding based on exceeding the upper bound
DI_graph <- ggplot(dataDI, aes(x = Iteration, y = DeviationDI, color = ExceedsUpperBoundDI)) +
  geom_point() +
  theme_minimal() +
  labs(title = "",
       x = "Simulation Iteration",
       y = "Deviation") +
  theme(axis.title.x = element_text(size = 14),  # Change x axis label text size
        axis.title.y = element_text(size = 14)) +  # Change y axis label text size
  scale_color_manual(values = c("TRUE" = "indianred3", "FALSE" = "dodgerblue"),  # Use red for exceeding, blue for not exceeding
                     name = "",
                     labels = c("TRUE" = "Loss of Equilibrium", "FALSE" = "Nash Equilibrium"))
# Changing text size of legend elements
DI_graph2 <- DI_graph + theme(legend.title = element_text(size = 14),  # Change legend title size
                              legend.text = element_text(size = 14))   # Change legend text size
# Save simulations to output file
write.csv(dataDI, "Dosage-insensitive_simulation_results.csv", row.names = FALSE)
_______________________

## SIMULATION #3 - Testing nasheek DOSAGE-INSENSITIVE simulation (range = 0-0.1 per gene, 
## epsilon upper bound = 0.1 x 3 = 0.3):
# Define the deviation function
deviationDS <- function(cX, cY, cZ) {
  abs(cX - cY) + abs(cX - cZ) + abs(cY - cZ)
}
# Define the epsilon range
epsilon_lowerDS <- 0.0
epsilon_upperDS <- 0.3
# Generate simulated concentrations
# Generate a random seed based on the current time and print it
random_seedDS <- as.integer(Sys.time())
print(random_seedDS)
set.seed(random_seedDS)
cX_valuesDS <- runif(1000, min = 0.1, max = 1.0)
cY_valuesDS <- runif(1000, min = 0.1, max = 1.0)
cZ_valuesDS <- runif(1000, min = 0.1, max = 1.0)
# Calculate deviations
deviationsDS <- mapply(deviationDS, cX_valuesDS, cY_valuesDS, cZ_valuesDS)
# Check if deviations fall within the epsilon range
within_epsilonDS <- deviationsDS > epsilon_upperDS  # This checks if the deviation exceeds the upper bound
# Counts number of deviations within epsilon range and prints to screen
within_epsilonDI <- deviationsDI >= epsilon_lowerDI & deviationsDI <= epsilon_upperDI
count_within_epsilonDI <- sum(within_epsilonDI)
print(paste("Number of instances within the epsilon range:", count_within_epsilonDI))
# Create a data frame for plotting with the corrected condition
dataDS <- data.frame(Iteration = 1:1000, DeviationDS = deviationsDS, ExceedsUpperBoundDS = within_epsilonDS)
# Plot the results with color coding based on exceeding the upper bound
DS_graph <- ggplot(dataDS, aes(x = Iteration, y = DeviationDS, color = ExceedsUpperBoundDS)) +
  geom_point() +
  theme_minimal() +
  labs(title = "",
       x = "Simulation Iteration",
       y = "Deviation") +
  theme(axis.title.x = element_text(size = 14),  # Change x axis label text size
        axis.title.y = element_text(size = 14)) +  # Change y axis label text size
  scale_color_manual(values = c("TRUE" = "indianred3", "FALSE" = "dodgerblue"),  # Use red for exceeding, blue for not exceeding
                     name = "",
                     labels = c("TRUE" = "Loss of Equilibrium", "FALSE" = "Nash Equilibrium"))
# Changing text size of legend elements
DS_graph2 <- DS_graph + theme(legend.title = element_text(size = 14),  # Change legend title size
                              legend.text = element_text(size = 14))   # Change legend text size
# Save simulations to output file
write.csv(dataDS, "Dosage-sensitive_simulation_results.csv", row.names = FALSE)

#Display multipanel in window
grid.arrange(DS_graph2, DI_graph2, nrow=1)
#Save multiplanel plot
g <- arrangeGrob(DS_graph2, DI_graph2, nrow=1)
ggsave(file="Simulations_multipanel.tiff", dpi=300, g)
