

# =================================
# Discrete probability
# =================================


# (Bernoulli) Simulate tossing a coin with probability of heads p. 

p = 0.5;
U = runif(1, min = 0, max = 1);
X = (U < p);


# (Coin Toss Simulation) Write codes to simulate tossing a fair coin to see how the law of large numbers works.

n = 1000;
U = runif(n, min = 0, max = 1);
toss = U < 0.5;
a = numeric(n + 1);
avg = numeric(n);
for(i in 2 : n + 1)
{
a[i] = a[i − 1] + toss[i − 1];
avg[i − 1] = a[i]/(i − 1);
}
plot(1 : n, avg[1 : n], type = ”l”, lwd = 5, col = ”blue”, ylab = ”P roportionofHeads”,
xlab = ”CoinT ossNumber”, cex.main = 1.25, cex.lab = 1.5, cex.axis = 1.75)


# (Binomial) Generate a Binomial(50, 0.2) random variable.

p = 0.2;
n = 50;
U = runif(n, min = 0, max = 1);
X = sum(U < p);



# Give an algorithm to simulate the value of a random variable X such that

P = c(0.35, 0.5, 0.9, 1);
X = c(1, 2, 3, 4);
counter = 1;
r = runif(1, min = 0, max = 1);
while(r > P[counter])
counter = counter + 1;
end
X[counter]



# =================================
# Continuous probability
# =================================


# (Exponential) Generate an Exponential(1) random variable

U = runif(1, min = 0, max = 1);
X = −log(U)

# (Gamma) Generate a Gamma(20,1) random variable.

n = 20;
lambda = 1;
X = (−1/lambda) ∗ sum(log(runif(n, min = 0, max = 1)));


# (Poisson) Generate a Poisson random variable. Hint: In this example, use the fact that the number of events in the interval [0, t] has Poisson distribution when the elapsed times between the events are Exponential.


Lambda = 2;
i = 0;
U = runif(1, min = 0, max = 1);
Y = −(1/Lambda) ∗ log(U);
sum = Y ;
while(sum < 1)
{U = runif(1, min = 0, max = 1);
Y = −(1/Lambda) ∗ log(U);
sum = sum + Y ;
i = i + 1; }
X = i



# (Box-Muller) Generate 5000 pairs of normal random variables and plot both histograms.

n = 5000;
U1 = runif(n, min = 0, max = 1)
U2 = runif(n, min = 0, max = 1)
Z1 = sqrt(−2 ∗ log(U1)) ∗ cos(2 ∗ pi ∗ U2)
Z2 = sqrt(−2 ∗ log(U1)) ∗ sin(2 ∗ pi ∗ U2)
hist(Z1, col = ”wheat”, label = T)













############################################################
# Example 1: Simulating a Bernoulli Trial using runif()
############################################################

# Generate one random number between 0 and 1
U = round(runif(1, min = 0, max = 1), digits = 1)
U

# Probability of success
p = 0.5

# Create an empty vector to store results
y <- numeric()

# Repeat the Bernoulli simulation 1000 times
for (i in 1:1000) {

  set.seed(i)                     # Set seed for reproducibility
  U = runif(1, min = 0, max = 1)  # Generate a uniform random number
  
  X = (U < p)                     # Bernoulli outcome (TRUE = success)
  
  y <- append(y, X)               # Store result in vector
}

############################################################
# Example 2: Law of Large Numbers with Coin Toss Simulation
############################################################

n = 1000                          # Number of coin tosses

# Generate n uniform random numbers
U = runif(n, min = 0, max = 1)

# Simulate coin tosses (TRUE = Heads, FALSE = Tails)
toss = U < 0.5

# Vector to store cumulative number of heads
a = numeric(n + 1)

# Vector to store running proportion of heads
avg = numeric(n)

# Compute cumulative heads and running average
for(i in 2:(n + 1)) {
  
  a[i] = a[i - 1] + toss[i - 1]   # Add current toss result
  avg[i - 1] = a[i] / (i - 1)     # Proportion of heads so far
}

# Plot the running proportion of heads
plot(
  1:n, avg[1:n],
  type = "l",                     # Line plot
  lwd = 5,                        # Line width
  col = "blue",
  ylab = "Proportion of Heads",
  xlab = "Coin Toss Number",
  cex.main = 1.25,
  cex.lab = 1.5,
  cex.axis = 1.2
)

############################################################
# Example 3: Gamma Distribution Simulation using Exponential
############################################################

n = 20
lambda = 1

# Generate one Gamma random variable
x = (-1 / lambda) * sum(log(runif(n, min = 0, max = 1)))
x

# Store multiple Gamma samples
gamma <- numeric()

# Repeat simulation 100 times
for (i in 1:100) {

  n = 20
  lambda = 1
  
  x = (-1 / lambda) * sum(log(runif(n, min = 0, max = 1)))
  
  gamma <- append(gamma, x)
}

############################################################
# Example 4: Plotting the Gamma Simulation
############################################################

# Base R histogram
hist(
  gamma,
  main = "Histogram of Simulated Gamma Values",
  xlab = "Gamma Values",
  col = "lightblue",
  border = "black"
)

############################################################
# Example 5: Using ggplot2 for Visualization
############################################################

library(ggplot2)

# Convert vector to data frame for ggplot
df <- data.frame(gamma)

# Histogram using ggplot2
ggplot(df, aes(x = gamma)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Histogram of Simulated Gamma Distribution",
    x = "Gamma Values",
    y = "Frequency"
  )