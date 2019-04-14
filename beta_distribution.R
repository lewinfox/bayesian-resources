beta <- function(alpha, beta) {
  # This Beta function calculates a normalising constant for the Beta
  # distribution. This is derived from the Gamma function which I don't
  # understand
  (factorial(alpha - 1) * factorial(beta - 1)) / factorial(alpha + beta - 1)
}

beta_dist <- function(x, alpha, beta) {
  # Given shape parameters alpha and beta, what is the value of the beta
  # distribution at point x? Alpha is the number of observed successes (or, more
  # generally, instances of one class), and Beta is the number of observed
  # instances of "not Alpha".
  (x^(alpha - 1) * (1 - x)^(beta - 1)) / beta(alpha, beta)
}

plot_beta_dist <- function(alpha, beta) {
  # Given an alpha and beta, plot the distribution from p = 0 to p = 1
  p <- seq(0, 1, 0.01)
  plot(p, beta_dist(p, alpha, beta), type = "l")
}

# Let's say we want to check whether a coin is fair (i.e. if p(heads) = 0.5 or
# not). After 30 flips we have 11 heads. Based on this data, what is a likely
# value for p? By plugging different values for p into the Beta distribution
# with alpha = 11 abd beta = (30 - 11) = 19, we can see where the peak of the
# probability density function is.
plot_beta_dist(11, 19)

# The function peaks around p = 0.4. However, it is not helpful to ask what the
# probability is that p = 0.4, because the probability that p is exactly a
# given value is 0. Instead we should ask "what is the probability that p lies
# between 0.395 and 0.405?" (for example). We need to specify a range. This means
# that we need to integrate the Beta function over our target range for p.

# This can be achieved using the pbeta function (CDF). The PDF is the derivative of
# the CDF
plot(seq(0, 1, 0.01), pbeta(seq(0, 1, 0.01), 11, 19), type = "l")

# Probability of p lying between 0.395 and 0.405
pbeta(0.405, 11, 19) - pbeta(0.395, 11, 19)
# > 0.04

# Probability of p lying between 0.3 and 0.5
pbeta(0.5, 11, 19) - pbeta(0.3, 11, 19)
# > 0.70


# qbeta allows us to answer questions like "where is the median of the function"
qbeta(0.5, 11, 19) # Median
qbeta(0.25, 11, 19) # Lower quartile
qbeta(0.75, 11, 19) # Upper quartile


# Incorporating priors
# If we have prior belief about the properties of the distribution, we can
# add the Beta distribution of those priors on to the distriution obtained from
# the experiement B(a_post, b_post) = B(a_obs + a_prior, b_obs + b_prior).
# The "strength" of the distribution (pointiness / certainty) increases with the
# absolute value of a and b, so for a given ratio of a and b the values chosen
# should reflect our confidence in the data.
# e.g.
plot_beta_dist(3, 7)    # Wide
plot_beta_dist(30, 70)  # Narrow

# Let's modify the example above by adding priors of differing strength that
# indicate that we believe the coin to be fair
x <- seq(0, 1, 0.01)

# First, the observed data and the priors separately
plot_beta_dist(11, 19)
lines(x, beta_dist(x, 2, 2), col = "red") #
lines(x, beta_dist(x, 8, 8), col = "blue")
lines(x, beta_dist(x, 16, 16), col = "green")

# Then the impact of adding the priors into the observed data
plot_beta_dist(11, 19)
lines(x, beta_dist(x, 11 + 2, 19 + 2), col = "red")
lines(x, beta_dist(x, 11 + 8, 19 + 8), col = "blue")
lines(x, beta_dist(x, 11 + 16, 19 + 16), col = "green")
# Note how the priors shift the distribution towards 0.5 and narrow it.


# Bayesian A/B testing
# From https://www.countbayesie.com/blog/2015/4/25/bayesian-ab-testing

# We are running an A/B test on emails. Both were sent to 150 people and the
# results are:
# A: 36 opens
# B: 50 opens
plot_beta_dist(36, 114)
lines(seq(0, 1, 0.01), beta_dist(seq(0, 1, 0.01), 50, 100), type = "l", col = "red")

# Cool! B is better than A. But there is some overlap in the distributions so
# we can't be too sure about the results. We can become more confident about the
# results by using Monte Carlo simulation to sample from these distributions,
# and by incorporating some prior belief (from intuition, experence). Let's say
# that from previous experience we think that the open rate for emails is about
# 30%. There are an infinite number of ways of representing that as a fraction,
# and a shown above the magnitude of the numbers we choose will affect the
# strength of the priors. Let's use 3/10 so alpha = 3 and beta = 7
n_trials <- 1e6
prior_alpha <- 3
prior_beta <- 7
a_samples <- rbeta(n_trials, 36 + prior_alpha, 114 + prior_beta)
b_samples <- rbeta(n_trials, 50 + prior_alpha, 100 + prior_beta)

plot_beta_dist(36, 114)
lines(seq(0, 1, 0.01), beta_dist(seq(0, 1, 0.01), 36 + prior_alpha, 114 + prior_beta), col = "red")

prob_b_better <- sum(b_samples > a_samples) / n_trials

# This tells us that B is 96% likely to be better than A. But how much better?
# Plotting the distribution of b_samples / a_samples will give an idea
plot(density(b_samples / a_samples))

# Again, to make more concrete statements about the improvements we can look
# at the cumulative distribution
ecdf_fun <- ecdf(b_samples / a_samples)
plot(seq(0, 3, 0.01), ecdf_fun(seq(0, 3, 0.01)), type = "l")
# What is the 95% confidence interval?
quantile(b_samples / a_samples, 0.975)
quantile(b_samples / a_samples, 0.025)
# What is the median improvement?
quantile(b_samples / a_samples, 0.5)
mean(b_samples / a_samples)
