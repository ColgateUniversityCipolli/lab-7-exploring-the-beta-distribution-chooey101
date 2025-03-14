library(tidyverse)
library(ggplot2)
#Answer to question: the mean comes from the first equation listed while the standard deviation is derived from the second equation
# Data 
data <- c(2944, 3206, 2751, 3089, 3406, 3275, 2606, 2723, 2475, 2930,
          2530, 2399, 2806, 2827, 2951, 2854, 2930, 2565, 2799, 3102,
          3454, 4185, 3095, 3247, 3371, 3302, 3544, 3454, 3468, 3233,
          2571, 3268, 2792, 2916, 3006, 3523, 2958, 3123, 2930, 2689,
          3075, 3337, 3468, 3254, 3061, 3647, 3295, 2971, 3068, 3371)

# Number of observations
n <- length(data)

# (1) MOM Estimates 
mu_mom <- mean(data)
sigma_mom <- sqrt(sum((data - mu_mom)^2) / (n - 1))

# (2) MLE Estimates (using denominator n for variance)
mu_mle <- mu_mom
sigma_mle <- sqrt(sum((data - mu_mle)^2) / n)

#  tibble with the estimates
estimates <- tibble(
  Method = c("MOM", "MLE"),
  mu = c(mu_mom, mu_mle),
  sigma = c(sigma_mom, sigma_mle)
)

# Output the estimates tibble
estimates

# (3) Plotting the data with estimated densities
# Create a tibble for the data
df <- tibble(value = data)

# Create a sequence for x values for plotting the density curves
x_vals <- seq(min(data), max(data), length.out = 200)

# Create data frames for the density curves for MOM and MLE
df_mom <- tibble(x = x_vals, density = dnorm(x_vals, mean = mu_mom, sd = sigma_mom), 
                 Method = "MOM")
df_mle <- tibble(x = x_vals, density = dnorm(x_vals, mean = mu_mle, sd = sigma_mle), 
                 Method = "MLE")

# Combine the two density data frames
df_density <- bind_rows(df_mom, df_mle)

# Plot the histogram with superimposed density curves
ggplot(df, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "lightblue", color = "black", alpha = 0.5) +
  geom_line(data = df_density, aes(x = x, y = density, color = Method, linetype = Method), size = 1) +
  labs(title = "Histogram of Tensile Strength Data with Estimated Normal Densities",
       x = "Tensile Strength (MPa)",
       y = "Density") +
  theme_minimal()