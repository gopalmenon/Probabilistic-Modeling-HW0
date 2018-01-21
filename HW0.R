# Return the pth quantile for distribution of Y
get_Y_quantile <- function(quantile_value, exponential_rate) {
  return(sqrt(-1* log(1 - quantile_value) / exponential_rate))
}

# Random number generation function
generate_random_number <- function(how_many, exponential_rate) {
  
  # Genrate a uniform distribution between 0 and 1
  uniform_distribution = runif(how_many, min=0, max=1)
  
  # Declare an array to store the random numbers from the distribution of Y
  ramdoms_from_exp_distr = numeric(how_many)
 
  # For each number in the uniform distribution find the quantile of Y
  for (unif_distr_counter in 1:length(uniform_distribution)) {
    ramdoms_from_exp_distr[unif_distr_counter] = get_Y_quantile(uniform_distribution[unif_distr_counter], exponential_rate)
  }
  
  return(ramdoms_from_exp_distr)
   
}

# Return probability density of Y
y_probability_density <- function(real_number, exponential_rate) {
  
  return (2*real_number*exponential_rate*exp(-1 * exponential_rate * real_number^2))
  
}


#Generate 10,000 realizations of the random variable Y with λ= 2
realizations_needed = 10000
exponential_rate_lambda = 2

random_numbers = generate_random_number(how_many = realizations_needed, exponential_rate = exponential_rate_lambda)

# Plot a histogram of these numbers, using the hist function with option freq = FALSE.
hist(random_numbers, freq=FALSE, main= "Random Number Generation", xlab="Random Number", ylab="Density", col="blue")

# Use the lines command to plot the pdf of Y on top.
number_sequence = seq(0,2,.001)
y_probability_density_trend = y_probability_density(real_number = number_sequence, exponential_rate = exponential_rate_lambda)
length(y_probability_density_trend)

lines(number_sequence, y_probability_density_trend, lty=1, lwd=3, col="red")
legend("topright", legen=c("Random Number Density", "PDF of function Y"), col=c("blue", "red"), lwd=3, lty=1)

## Compute the sample mean and variance of your 10,000 realizations
mean(random_numbers)
var(random_numbers)