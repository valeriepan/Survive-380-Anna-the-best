
# A basic version, without any functions yet
# add some adjustable parameters when we make the function

# 1. generate n binomial samples
set.seed(1)
n <- 10^4 # number of simulated values

# just chose some random values for now
m <- 10 # total trials in binomial 
p_true <- 0.5 # true probability of success in the binomial population

x <- rbinom(n, size = m, p = p_true) # draw a random sample x with n number of simulated binomial values

p_0 <- 0.7 # define some null hypothesis proportion

# 2. Apply z-test to each sample
for(i in 1:n){
  p_hat <- x[i]/m # sample proportion, probability of sample successes / number of bin trials  
  
  z <- (p_hat - p_0) / sqrt(p_0*(1-p_0)/m) # calculate z-test statistic
  
  # add the hypothesis test here...
  
}


