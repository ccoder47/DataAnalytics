# CS544 Assignment 4 - Clarence Audate

# Part 1: Binomial distribution
p1 <- 0.4
n <- 4
x_values <- 0:n  # possible number of batters struck out (0 through 4)

# Compute PMF for each x in 0..4 when p = 0.4:
pmf_p1 <- dbinom(x_values, size = n, prob = p1)
# Compute CDF for each x in 0..4 when p = 0.4 (cumulative probability up to x):
cdf_p1 <- pbinom(x_values, size = n, prob = p1)
# PMF values for p=0.4 (for X=0 to 4):
pmf_p1  # [1] 0.1296 0.3456 0.3456 0.1536 0.0256
# CDF values for p=0.4 (cumulative up to X):
cdf_p1  # [1] 0.1296 0.4752 0.8208 0.9744 1.0000

# Plot the PMF and CDF for p = 0.4 side by side:
par(mfrow=c(1,2))  # two plots in one row
# PMF plot for p=0.4
barplot(pmf_p1, names.arg = x_values,
        main = paste("Binomial(n=", n, ", p=", p1, ") PMF"),
        xlab = "Number of batters struck out (X)", ylab = "P(X = x)",
        col = "skyblue")
# CDF plot for p=0.4 (stepwise graph)
plot(x_values, cdf_p1, type="s",
     main = paste("Binomial(n=", n, ", p=", p1, ") CDF"),
     xlab = "Number of batters struck out (X)", ylab = "P(X \u2264 x)",
     col = "blue")
par(mfrow=c(1,1))  # reset plotting layout

# Repeat for p = 0.8
p2 <- 0.8
# Compute PMF and CDF for each x when p = 0.8:
pmf_p2 <- dbinom(x_values, size = n, prob = p2)
cdf_p2 <- pbinom(x_values, size = n, prob = p2)
# PMF values for p=0.8 (X=0 to 4):
pmf_p2  # [1] 0.0016 0.0256 0.1536 0.4096 0.4096
# CDF values for p=0.8:
cdf_p2  # [1] 0.0016 0.0272 0.1808 0.5904 1.0000

# Plot PMF and CDF for p = 0.8:
par(mfrow=c(1,2))
barplot(pmf_p2, names.arg = x_values,
        main = paste("Binomial(n=", n, ", p=", p2, ") PMF"),
        xlab = "Number of batters struck out (X)", ylab = "P(X = x)",
        col = "lightgreen")
plot(x_values, cdf_p2, type="s",
     main = paste("Binomial(n=", n, ", p=", p2, ") CDF"),
     xlab = "Number of batters struck out (X)", ylab = "P(X \u2264 x)",
     col = "green")
par(mfrow=c(1,1))  # reset layout

# From the shapes of the distributions:
# - For p = 0.4, the distribution is skewed toward fewer strikeouts (0 or 1 are most likely).
# - For p = 0.8, the distribution is skewed toward more strikeouts (3 or 4 are most likely).
# The CDF for p = 0.8 rises more quickly, reflecting the higher probability of many successes.

# Part 2: Binomial distribution
# Scenario: Probability a bus arrives on time p = 0.7, with n = 12 buses.
n <- 12
p <- 0.7
# a) Compute P(X = 5) -- probability exactly 5 buses are on time.
P_X_eq_5 <- dbinom(5, size = n, prob = p)
P_X_eq_5  # P(X=5) ≈ 0.02911 (about 2.9%)
# b) Compute P(X <= 5) -- probability 5 or fewer buses are on time.
P_X_le_5 <- pbinom(5, size = n, prob = p)
P_X_le_5  # P(X<=5) ≈ 0.03860 (about 3.86%)
# c) Compute and plot the PMF for X = 0 to 12.
x_values <- 0:n
pmf_values <- dbinom(x_values, size = n, prob = p)
pmf_values  # PMF for X=0..12 (peak will be near X ≈ 8 or 9)
# Plot the PMF as a barplot:
barplot(pmf_values, names.arg = x_values,
        main = paste("Binomial PMF (n=", n, ", p=", p, ")"),
        xlab = "Number of on-time buses (X)", ylab = "P(X = x)",
        col = "orange")
# d) Plot both PMF and CDF for this binomial distribution.
par(mfrow=c(1,2))
# PMF (using a line/bar combination for clarity)
plot(x_values, pmf_values, type="h", lwd=2,
     main="Binomial PMF", xlab="X", ylab="P(X = x)",
     col="orange")
points(x_values, pmf_values, pch=16, col="orange")
# CDF (step function plot)
cdf_values <- pbinom(x_values, size = n, prob = p)
plot(x_values, cdf_values, type="s", lwd=2,
     main="Binomial CDF", xlab="X", ylab=expression(P(X <= x)),
     col="red")
par(mfrow=c(1,1))  # reset layout to one plot

# Part 3: Poisson distribution
# Scenario: Cars arrive at a rate of λ = 9 cars per hour (Poisson distribution for count per hour).
lambda <- 9
# a) Compute P(X = 6) -- probability exactly 6 cars arrive in an hour.
P_X_eq_6 <- dpois(6, lambda = lambda)
P_X_eq_6  # P(X=6) ≈ 0.09109 (about 9.11%)
# b) Compute P(X >= 6) -- probability 6 or more cars arrive.
# This is 1 - P(X <= 5).
P_X_ge_6 <- 1 - ppois(5, lambda = lambda)
P_X_ge_6  # P(X>=6) ≈ 0.88431 (about 88.43%)
# c) Compute P(3 <= X <= 6) -- probability between 3 and 6 cars (inclusive) arrive.
# This equals P(X <= 6) - P(X <= 2).
P_X_le_6 <- ppois(6, lambda = lambda)
P_X_le_2 <- ppois(2, lambda = lambda)
P_3_to_6 <- P_X_le_6 - P_X_le_2
P_3_to_6  # P(3<=X<=6) ≈ 0.20055 (about 20.05%)
# d) Plot the PMF for X = 0 to 20.
x_values <- 0:20
pmf_values <- dpois(x_values, lambda = lambda)
plot(x_values, pmf_values, type="h", lwd=2,
     main=paste("Poisson PMF (λ=", lambda, ")"),
     xlab="Number of cars (X)", ylab="P(X = x)",
     col="purple")
points(x_values, pmf_values, pch=16, col="purple")

# Part 4: Uniform distribution
# Discrete uniform distribution between 60 and 100 inclusive (i.e., X equally likely to be any integer 60,...,100).
values <- 60:100             # all possible values of X
N <- length(values)          # number of possible values (100 - 60 + 1 = 41)
# a) Compute P(X = 60), P(X = 80), P(X = 100).
P_X_eq_60 <- 1 / N
P_X_eq_80 <- 1 / N
P_X_eq_100 <- 1 / N
P_X_eq_60   # 1/41 ≈ 0.02439
P_X_eq_80   # 1/41 ≈ 0.02439
P_X_eq_100  # 1/41 ≈ 0.02439  (each value has equal probability)
# b) Compute the mean and standard deviation of X.
mean_X <- mean(values)  # should be (min+max)/2 = (60+100)/2 = 80
# Compute population standard deviation (using N in denominator):
sd_X <- sqrt(sum((values - mean_X)^2) / N)
mean_X  # Mean = 80
sd_X    # Std. deviation ≈ 11.83
# c) Compute P(X <= 70).
count_le_70 <- sum(values <= 70)     # number of values from 60 up to 70 inclusive
P_X_le_70 <- count_le_70 / N
P_X_le_70  # P(X<=70) = 11/41 ≈ 0.2683
# d) Compute P(X > 80).
count_gt_80 <- sum(values > 80)      # number of values greater than 80
P_X_gt_80 <- count_gt_80 / N
P_X_gt_80  # P(X>80) = 20/41 ≈ 0.4878
# e) Compute P(90 <= X <= 100).
count_90_to_100 <- sum(values >= 90 & values <= 100)  # number of values between 90 and 100
P_90_to_100 <- count_90_to_100 / N
P_90_to_100  # P(90<=X<=100) = 11/41 ≈ 0.2683

# Part 5: Normal distribution
# Scenario: Customer spending is approximately normal with mean = 60 and SD = 6.
mu <- 60
sigma <- 6
# a) Plot the PDF over the range [mean - 3*sd, mean + 3*sd] = [42, 78].
x_vals <- seq(mu - 3*sigma, mu + 3*sigma, length.out = 100)
pdf_vals <- dnorm(x_vals, mean = mu, sd = sigma)
plot(x_vals, pdf_vals, type="l", lwd=2,
     main=paste("Normal PDF (mean=", mu, ", sd=", sigma, ")"),
     xlab="X (customer spending)", ylab="Density",
     col="darkblue")
# b) Compute P(X < 20).
P_X_less_20 <- pnorm(20, mean = mu, sd = sigma)
P_X_less_20  # P(X < 20) ≈ 1.31e-11 (essentially 0)
# c) Compute P(20 <= X <= 50).
P_X_le_50 <- pnorm(50, mean = mu, sd = sigma)
P_20_to_50 <- P_X_le_50 - P_X_less_20
P_20_to_50  # P(20<=X<=50) ≈ 0.04779 (about 4.78%)
# (Since P(X < 20) is nearly 0, this is essentially P(X <= 50).)
# d) Compute the 10th and 90th percentiles (values cutting off the lowest 10% and highest 10% of the distribution).
P10 <- qnorm(0.10, mean = mu, sd = sigma)  # 10th percentile
P90 <- qnorm(0.90, mean = mu, sd = sigma)  # 90th percentile
P10  # ≈ 52.31
P90  # ≈ 67.69
# The middle 80% of the distribution lies roughly between ~52.3 and ~67.7 (the 10th to 90th percentile range).

# Part 6: Exponential distribution
# Scenario: Calls arrive at rate λ = 20 calls per hour. Convert to rate per minute for exponential (λ_per_min = 20/60 ≈ 0.3333).
rate_per_min <- 1/3  # calls per minute
# a) Compute P(X <= 3) where X is the waiting time (in minutes) until the next call.
P_X_le_3 <- pexp(3, rate = rate_per_min)
P_X_le_3  # P(X<=3) ≈ 0.63212 (about 63.2% chance of a call within 3 minutes)
# b) Compute P(3 <= X <= 6) = P(X <= 6) - P(X < 3).
P_X_le_6 <- pexp(6, rate = rate_per_min)
P_3_to_6 <- P_X_le_6 - P_X_le_3
P_3_to_6  # P(3<=X<=6) ≈ 0.23254 (about 23.3%)
# c) Plot the CDF of the exponential distribution (waiting time in minutes).
x_vals <- seq(0, 15, length.out = 100)  # time from 0 to 15 minutes
cdf_vals <- pexp(x_vals, rate = rate_per_min)
plot(x_vals, cdf_vals, type="l", lwd=2,
     main="Exponential CDF (rate = 1/3 per minute)",
     xlab="Time (minutes)", ylab=expression(P(X <= x)),
     col="darkgreen")

# Interpretation: ~63% of calls occur within 3 minutes; ~23% probability the wait is between 3 and 6 minutes.
# The CDF plot shows the rapid rise (calls arriving quickly) and then leveling off as it approaches 1 (as time goes to infinity).

