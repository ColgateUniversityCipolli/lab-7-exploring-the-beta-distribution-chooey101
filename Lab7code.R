library(tidyverse)
library(e1071)
library(patchwork)
library(nleqslv)
library(cumstats)
######Part 1########


#statistical summary
stat_funct <- function (alpha,beta) {
  tibble(
    alpha = alpha,
    beta = beta,
    mean =  alpha/(alpha+beta),
    variance = (alpha*beta)/(((alpha+beta)^2)*(alpha+beta+1)),
    skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta)),
    ex_kurt = (6*(((alpha-beta)^2)*(alpha+beta+1)-(alpha*beta)*(alpha+beta+2)))/((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))
  )
}



beta_table <- tibble(
alpha = double(),
beta = double(),
mean = double(),
variance = double(),
skewness = double(),
ex_kurt = double()
)

plot_funct <- function (alpha,beta) {
  tibble(
    alpha = alpha,
    beta = beta,
    mean =  alpha/(alpha+beta),
    variance = (alpha*beta)/(((alpha+beta)^2)*(alpha+beta+1)),
    skewness = (2*(beta-alpha)*sqrt(alpha+beta+1))/((alpha+beta+2)*sqrt(alpha*beta)),
    ex_kurt = (6*(((alpha-beta)^2)*(alpha+beta+1)-(alpha*beta)*(alpha+beta+2)))/((alpha*beta)*(alpha+beta+2)*(alpha+beta+3))
  )
  
  q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|> # generate a grid of points
    mutate(beta.pdf = dbeta(x, alpha, beta), # compute the beta PDF
           norm.pdf = dnorm(x, # Gaussian distribution with same mean and variance
                            mean = alpha/(alpha+beta), 
                            sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))
  
  ggplot(data= q1.fig.dat)+ # specify data
    geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) + # plot beta dist
    geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) + # plot gaussian dist
    geom_hline(yintercept=0)+ # plot x axis
    theme_bw()+ # change theme
    xlab("x")+ # label x axis
    ylab("Density")+ # label y axis
    scale_color_manual("", values = c("black", "grey"))+ # change colors
    theme(legend.position = "bottom") 
}
#Compute and graph summary for a=2, b=5
plot_funct(2,5)
plot_funct(5,5) #Compute and graph summary for a=5, b=5
plot_funct(5,2)
plot_funct(0.5,0.5) #Compute and graph summary for a=.5, b=.5

stat25 <- stat_funct(2,5) #Compute and graph summary for a=2, b=5
stat55 <- stat_funct(5,5) #Compute and graph summary for a=5, b=5
stat52 <- stat_funct(5,2)
stat.5 <- stat_funct(0.5,0.5) #Compute and graph summary for a=.5, b=.5

beta_table <- rbind(beta_table, stat25)
beta_table <- rbind(beta_table, stat55)
beta_table <- rbind(beta_table, stat52)
beta_table <- rbind(beta_table, stat.5)

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
graph_funct(2,5)
#################################
#######Task 4########
#################################
true_stats <- stat_funct(2, 5)
set.seed(7272 + i)
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


#############################
#Task 4.5---Using Loops
#############################
true_stats <- stat_funct(2, 5)
set.seed(7272)
beta_values <- rbeta(500, shape1 = 2, shape2 = 5) #beta distribution for alpha=5 and beta=2
cum_beta <- cummean(beta_values)
cum_variance  <- cumvar(beta_values)
cum_skewness  <- cumskew(beta_values)
cum_kurtosis  <- cumkurt(beta_values) - 3

df <- data.frame(
  Index = 1:500,
  Mean = cum_beta,
  Variance = cum_variance,
  Skewness = cum_skewness,
  Ex_kurt = cum_kurtosis
)
# Initial empty plots
p_mean <- ggplot() + theme_minimal() +
  labs(title = "Cumulative Mean", x = "Observation", y = "Mean") +
  geom_hline(yintercept = true_stats$mean, color = "red", linetype = "dashed")

p_var <- ggplot() + theme_minimal() +
  labs(title = "Cumulative Variance", x = "Observation", y = "Variance") +
  geom_hline(yintercept = true_stats$variance, color = "red", linetype = "dashed")

p_skew <- ggplot() + theme_minimal() +
  labs(title = "Cumulative Skewness", x = "Observation", y = "Skewness") +
  geom_hline(yintercept = true_stats$skewness, color = "red", linetype = "dashed")

p_kurt <- ggplot() + theme_minimal() +
  labs(title = "Cumulative Kurtosis (Excess)", x = "Observation", y = "Kurtosis") +
  geom_hline(yintercept = true_stats$ex_kurt, color = "red", linetype = "dashed")

# Simulate and plot cumulative statistics for iterations 2 through 50
for(i in 2:50){
  set.seed(7272 + i)
  beta_values <- rbeta(500, shape1 = 2, shape2 = 5)
  
  df_iter <- data.frame(
    Index = 1:500,
    Mean = cummean(beta_values),
    Variance = cumvar(beta_values),
    Skewness = cumskew(beta_values),
    Kurtosis = cumkurt(beta_values) - 3
  )
  
  # Add lines for each iteration to plots with varying colors
  p_mean <- p_mean +
    geom_line(data = df_iter, aes(x = Index, y = Mean), color = i, alpha = 0.6)
  
  p_var <- p_var +
    geom_line(data = df_iter, aes(x = Index, y = Variance), color = i, alpha = 0.6)
  
  p_skew <- p_skew +
    geom_line(data = df_iter, aes(x = Index, y = Skewness), color = i, alpha = 0.6)
  
  p_kurt <- p_kurt +
    geom_line(data = df_iter, aes(x = Index, y = Kurtosis), color = i, alpha = 0.6)
}

# Combine plots using patchwork
(p_mean | p_var) / (p_skew | p_kurt)
sim_results <- tibble(
  mean = numeric(),
  variance = numeric(),
  skewness = numeric(),
  kurtosis = numeric()
)
#################################
#######Task 5########
#################################
for (i in 1:1000) {
  set.seed(7272+i)
  beta_values <- rbeta(500, shape1 = 2, shape2 = 5)
  
  # Calculate statistics
  sim_results <- sim_results |>
    add_row(
      mean = mean(beta_values),
      variance = var(beta_values),
      skewness = cumstats::skewness(beta_values),
      kurtosis = cumstats::kurtosis(beta_values) - 3) # Excess kurtosis
}

# Plotting histograms with estimated density curves:

library(ggplot2)

# Mean histogram and density
p_mean <- ggplot(sim_results, aes(x=mean)) +
  geom_histogram(aes(y=..density..), fill="steelblue", alpha=0.5, bins=30) +
  geom_density(color="blue") +
  geom_vline(xintercept = true_stats$mean, color="red", linetype="dashed") +
  theme_minimal() +
  labs(title = "Mean", x = "Mean", y = "Density")

# Variance plot
p_variance <- ggplot(sim_results, aes(x=variance)) +
  geom_histogram(aes(y=..density..), bins=30, fill="green", alpha=0.5) +
  geom_density(color="darkgreen") +
  geom_vline(xintercept = true_stats$variance, color="red", linetype="dashed") +
  theme_minimal() +
  labs(title = "Variance", x = "Variance", y = "Density")

# Skewness
p_skewness <- ggplot(sim_results, aes(x=skewness)) +
  geom_histogram(aes(y=..density..), fill="purple", alpha=0.6, bins=30) +
  geom_density(color="purple") +
  geom_vline(xintercept = true_stats$skewness, color="red", linetype="dashed") +
  theme_minimal() +
  labs(title = "Skewness", x = "Skewness", y = "Density")

# Kurtosis
p_kurtosis <- ggplot(sim_results, aes(x=kurtosis)) +
  geom_histogram(bins=30, aes(y=..density..), fill="orange") +
  geom_density(color="brown") +
  geom_vline(xintercept = true_stats$ex_kurt, color="red", linetype="dashed") +
  theme_minimal() +
  labs(title = "Kurtosis (Excess)", x = "Kurtosis", y = "Density")
p_mean
p_variance
p_skewness
p_kurtosis

############
#Lab 8
############

##Task 6##
wb.data <- read_csv("wbdata.csv") 
wb.data <- wb.data |>
  select(67) |>
  rename("2022_data" = "...67")|>
  mutate(`2022_data` = if_else(row_number() %in% 2:267, `2022_data` / 1000, `2022_data`))


##Task 7##

#Method of Moments Function
MOM.beta.fn <- function(data, par){
  alpha <- par[1]
  beta <- par[2]
  
  EX1 <- (alpha)/(alpha+beta)
  EX2 <- ((alpha+1)*alpha)/((alpha+beta+1)*(alpha+beta))
  m1 <- mean(data, na.rm = T)
  m2 <- mean(((data)^2), na.rm = T)
  estimates <- c(EX1-m1,EX2-m2)
  return (estimates) # Goal: find lambda so this is 0
}
#Solve for MOM
mom_result <- nleqslv(x=c(1,1),
        fn = MOM.beta.fn,
        data=wb.data$`2022_data`)

alpha_mom <- mom_result$x[1]
beta_mom <- mom_result$x[2]

data_df <- data.frame(x = wb.data$`2022_data`)

ggplot(data = data_df, aes(x = x)) +
  # Plot the histogram
  geom_histogram(aes(y = ..density..),bins=30
                 , fill = "lightblue", color = "black") +
  
  # Add the MOM Beta distribution
  stat_function(
    fun = function(x) dbeta(x, shape1 = alpha_mom, shape2 = beta_mom),
    color = "red", size = 1.5
  ) +
  
  # Customize the plot
  labs(
    title = "Histogram of Data with MOM Beta Distribution",
    x = "Data Values",
    y = "Density"
  ) +
  theme_minimal()

#MLE Function
llbeta <- function(par, data, neg=FALSE){
  alpha <- par[1]
  beta <- par[2]  
  loglik <- sum(log(dbeta(x=data, shape1 = alpha, shape2 = beta)), na.rm=T)
  
  return(ifelse(neg, -loglik, loglik))
}

###Solve for MLE
optim(par = c(1,1),
      fn = llbeta,
      data = wb.data$`2022_data`,
      neg = F)



#############
# Task 8 -- Which estimators should we use?
#############
pt.est <- tibble(
  iteration = numeric(),
  MLE.alpha = numeric(),
  MLE.beta = numeric(),
  MOM.alpha = numeric(),
  MOM.beta = numeric()
)
for(i in 1:1000){
  set.seed(7272 + i)
  beta_estimation_values <- rbeta(266, shape1 = 8, shape2 = 950)
  MLE.est = optim(par = c(8,950),
                  fn = llbeta,
                  data = beta_estimation_values,
                  neg = T)
  MLE.alpha = MLE.est$par[1]
  MLE.beta = MLE.est$par[2]
  
  MOM.est = nleqslv(x=c(8,950),
                    fn = MOM.beta.fn,
                    data = beta_estimation_values)
  MOM.alpha = MOM.est$x[1]
  MOM.beta = MOM.est$x[2]
  
  pt.est <- pt.est |>
    add_row(
      iteration = i,
      MLE.alpha = MLE.alpha,
      MLE.beta = MLE.beta,
      MOM.alpha = MOM.alpha, 
      MOM.beta = MOM.beta
      
    )
}

p_mle_alpha <- ggplot(pt.est, aes(x=MLE.alpha))+
  geom_histogram(aes(y=after_stat(density)), fill="steelblue", alpha=0.5, bins=30) +
  geom_density(color="blue") +
  theme_minimal() +
  labs(title = "MLE Alpha", x = "MLE Alpha", y = "Density")

p_mle_beta <- ggplot(pt.est, aes(x=MLE.beta))+
  geom_histogram(aes(y=after_stat(density)), fill="purple", alpha=0.5, bins=30) +
  geom_density(color="red") +
  theme_minimal() +
  labs(title = "MLE Beta", x = "MLE Beta", y = "Density")

p_mom_alpha <- ggplot(pt.est, aes(x=MOM.alpha))+
  geom_histogram(aes(y=after_stat(density)), fill="orange", alpha=0.5, bins=30) +
  geom_density(color="brown") +
  theme_minimal() +
  labs(title = "MOM Alpha", x = "MLE Alpha", y = "Density")

p_mom_beta <- ggplot(pt.est, aes(x=MOM.beta))+
  geom_histogram(aes(y=after_stat(density)), fill="green", alpha=0.5, bins=30) +
  geom_density(color="darkgreen") +
  theme_minimal() +
  labs(title = "MOM Beta", x = "MLE Beta", y = "Density")


(p_mle_alpha | p_mle_beta) / (p_mom_alpha | p_mom_beta)

true_alpha = 8
true_beta = 950

#Calculation of Bias
stats.tib <- tibble(
Method = character(),
Bias = numeric(),
Precision = numeric(),
MSE = numeric()
)
new.rows <- tibble(
Method = c("MLE Alpha","MLE Beta","MOM Alpha","MOM Beta"),

Bias = c(
  mean(pt.est$MLE.alpha, na.rm=TRUE) - true_alpha,
  mean(pt.est$MLE.beta, na.rm=TRUE) - true_beta,
  mean(pt.est$MOM.alpha, na.rm=TRUE) - true_alpha,
  mean(pt.est$MOM.beta, na.rm =TRUE) - true_beta
),

Precision = c(
1/var(pt.est$MLE.alpha),
1/var(pt.est$MLE.beta),
1/var(pt.est$MOM.alpha),
1/var(pt.est$MOM.beta)
),

MSE = c(
var(pt.est$MLE.alpha) + (mean(pt.est$MLE.alpha, na.rm=TRUE) - true_alpha)^2,
var(pt.est$MLE.beta) + (mean(pt.est$MLE.beta, na.rm=TRUE) - true_beta)^2,
var(pt.est$MOM.alpha) + (mean(pt.est$MOM.alpha, na.rm=TRUE) - true_alpha)^2,
var(pt.est$MLE.beta) + (mean(pt.est$MLE.beta, na.rm=TRUE) - true_beta)^2
)
)

stats.tib <- bind_rows(stats.tib, new.rows)

















