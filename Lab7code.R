library(tidyverse)
library(e1071)
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
  
  tibble(x=beta.sample) |>
    summarize(
      mean = mean(x),
      variance = var(x),
      skewness = skewness(x),
      ex_kurt = (kurtosis(x))
    )
  
  histogram <- hist(beta.sample)
  histogram 
  

  ggplot(data = beta.sample)+
  geom_density
    
}

















