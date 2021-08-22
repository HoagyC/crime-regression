  ## Running some tests on the gmm library to understand how to implement custom regressions using Generalized Methods of Moments

library(gmm)

# Setting up a generator for data that looks similar to our expected crime data, to see how well gmm recovers the coefficients
make.test.crime.data <- function(t, n){
  individual.effects <- rnorm(n, mean=50, sd=20)
  arrest.values <- matrix(rnorm(n*(t+100), mean=20, sd=10),nrow=t + 100)
  auto.coef <- 0.8
  arrest.coef <- 0.25
  crime.data <- matrix(individual.effects + rnorm(n, mean = 10), nrow=1)
  for (i in 2:(t + 100)) {
    new.crime <- 0.9 * tail(crime.data, 1) + arrest.values[i-1, ] + individual.effects + rnorm(n, sd=10) 
    crime.data <- rbind(crime.data, new.crime)
  }
  print(crime.data)
  return(tail(crime.data, t))
}

g.test <- function(theta, x){
  
}

# Textbook example using the first three moments of the normal distribution
g1 <- function(tet, x)
{
  m1 = (tet[1] - x)
  m2 = (tet[2]^2 - (x - tet[1])^2)
  m3 = x^3-tet[1]*(tet[1]^2 + 3*tet[2]^2)
  f = cbind(m1, m2, m3)
  return(f)
}

set.seed(123)   # Setting the seed ensure the same results each run from 'random' number generation
n <- 200        # Number of trials to run
x1 = rnorm(n, mean = 4, sd = 2)     # Generating example data
res <- gmm(g1, x1, t0=cbind(1, 1))  # Run gmm with the sample data and moment function

print(make.test.crime.data(10, 10))