library(tidyverse)
library(e1071)
libary(patchwork)
######Part 1########


#statistical summary
stat_funct <- function (alpha,beta) {
  tibble(
    mean =  alpha/(alpha+beta),
    variance = (alpha*beta)/(((alpha+beta)^2)*(alpha+beta+1)),
    skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta)),
    ex_kurt = (6*(((alpha-beta)^2)*(alpha+beta+1)-(alpha*beta)*(alpha+beta+2)))/((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))
  )
}

plot_funct <- function (alpha,beta) {
  tibble(
    mean =  alpha/(alpha+beta),
    variance = (alpha*beta)/(((alpha+beta)^2)*(alpha+beta+1)),
    skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta)),
    ex_kurt = (6*(((alpha-beta)^2)*(alpha+beta+1)-(alpha*beta)*(alpha+beta+2)))/((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))
  )
  
  q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta), # compute the beta PDF
           norm.pdf = dnorm(x, # Gaussian distribution with
                            mean = alpha/(alpha+beta), # same mean and variance
                            sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
  
  ggplot(data= q1.fig.dat)+ # specify data
    geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + # plot beta dist
    geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + # plot guassian dist
    geom_hline(yintercept=0)+ # plot x axis
    theme_bw()+ # change theme
    xlab("x")+ # label x axis
    ylab("Density")+ # label y axis
    scale_color_manual("", values = c("black", "grey"))+ # change colors
    theme(legend.position = "bottom") 
}
stat_funct(2,5) #Compute and graph summary for a=2, b=5
stat_funct(5,5) #Compute and graph summary for a=5, b=5
stat_funct(0.5,0.5) #Compute and graph summary for a=.5, b=.5

############Task 2-Compute the Moments##########
beta.moment <- function(alpha, beta, k, centered=TRUE){
  uncentered.integrand <- function (x) {(x^k)*dbeta(x,alpha,beta)}
  centered.integrand <- function (x) {((x-(alpha/(alpha+beta)))^k)*dbeta(x,alpha,beta)}
  if(centered){
    centered_moment <- integrate(centered.integrand, lower = 0, upper=1)$value
    centered_moment
  }
  else{
    uncentered_moment <- integrate(uncentered.integrand, lower=0, upper=1)$value
    uncentered_moment
  }
}

####stats for (a=2, b=5)

stats25 <- tibble(
  mean = beta.moment(2,5,1,centered=FALSE),
  variance = beta.moment(2,5,2,centered=TRUE),
  skewness = (beta.moment(2,5,3,centered=TRUE))/((beta.moment(2,5,2,centered=TRUE))^(3/2)),
  ex_kurt = ((beta.moment(2,5,4,centered=TRUE))/((beta.moment(2,5,2,centered=TRUE))^2))-3
  
)

stats55 <- tibble(
  mean = beta.moment(2,5,1,centered=FALSE),
  variance = beta.moment(2,5,2,centered=TRUE),
  skewness = (beta.moment(2,5,3,centered=TRUE))/((beta.moment(2,5,2,centered=TRUE))^(3/2)),
  ex_kurt = ((beta.moment(2,5,4,centered=TRUE))/((beta.moment(2,5,2,centered=TRUE))^2))-3
  
)

stats.5 <- tibble(
  mean = beta.moment(2,5,1,centered=FALSE),
  variance = beta.moment(2,5,2,centered=TRUE),
  skewness = (beta.moment(2,5,3,centered=TRUE))/((beta.moment(2,5,2,centered=TRUE))^(3/2)),
  ex_kurt = ((beta.moment(2,5,4,centered=TRUE))/((beta.moment(2,5,2,centered=TRUE))^2))-3
  
)

##################Task 3##########
graph_funct <- function(alpha, beta){
  set.seed(7272) # Set seed so we all get the same results.
  sample.size <- 500 # Specify sample details
  beta.sample <- rbeta(n = sample.size, # sample size
                       shape1 = alpha, # alpha parameter
                       shape2 = beta) #beta
  data <- tibble(x = beta.sample)
  
  # Calculate summary statistics
  stats <- data |>
    summarize(
      mean = mean(x),
      variance = var(x),
      skewness = skewness(x),
      ex_kurtosis = (kurtosis(x) - 3)  # Excess kurtosis
    )
  
  plot <- ggplot(data, aes(x = x)) +
    geom_histogram(
      aes(y = after_stat(density)), 
      bins = 30, 
      fill = "lightblue", 
      color = "blue",
      alpha = 0.5  # Add some transparency
    ) +
    geom_density(color = "red", linewidth = 1) +
    labs(
      title = bquote("Beta Distribution (α = " ~ .(alpha) ~ ", β = " ~ .(beta) ~ ")"),
      x = "Value",
      y = "Density"
    ) +
    theme_minimal()
  
  # Combine all results into a list
  results <- list(
    sample = beta.sample,
    summary_stats = stats,
    plot = plot,
    alpha = alpha,
    beta = beta,
    sample_size = sample.size
  )
  
  return(results)
}
#################################
#######Task 4########
#################################
true_stats <- stat_funct(2, 5)
set.seed(7272+i)
beta_values <- rbeta(500, shape1 = 2, shape2 = 5) #beta distribution for alpha=5 and beta=2
cum_beta <- cumean(beta_values)
cum_variance  <- cumvar(beta_values)
cum_skewness  <- cumskewness(beta_values)
cum_kurtosis  <- cumkurtosis(beta_values) - 3

df <- data.frame(
  Index = 1:500,
  Mean = cum_beta,
  Variance = cum_variance,
  Skewness = cum_skewness,
  Ex_kurt = cum_kurtosis
)


# Individual plots with true value intercepts
p1 <- ggplot(df, aes(x = Index, y = Mean)) +
  geom_line(color = "steelblue") +
  geom_hline(yintercept = true_stats$mean, color = "red", linetype = "dashed") +
  labs(title = "Cumulative Mean", x = "Observation", y = "Mean") +
  theme_minimal()

p2 <- ggplot(df, aes(x = Index, y = Variance)) +
  geom_line(color = "darkgreen") +
  geom_hline(yintercept = true_stats$variance, color = "red", linetype = "dashed") +
  labs(title = "Cumulative Variance", x = "Observation", y = "Variance") +
  theme_minimal()

p3 <- ggplot(df, aes(x = Index, y = Skewness)) +
  geom_line(color = "purple") +
  geom_hline(yintercept = true_stats$skewness, color = "red", linetype = "dashed") +
  labs(title = "Cumulative Skewness", x = "Observation", y = "Skewness") +
  theme_minimal()

p4 <- ggplot(df, aes(x = Index, y = Kurtosis)) +
  geom_line(color = "orange") +
  geom_hline(yintercept = true_stats$ex_kurt, color = "red", linetype = "dashed") +
  labs(title = "Cumulative Kurtosis (Excess)", x = "Observation", y = "Kurtosis") +
  theme_minimal()

(p1 | p2) / (p3 | p4) #Creates 2x2 grid of plots using patchwork
#################################
#######Task 5########
#################################




















