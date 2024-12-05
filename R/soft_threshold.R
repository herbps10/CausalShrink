soft_threshold <- \(x, lambda) ifelse(x < -lambda, x + lambda, ifelse(x > lambda, x - lambda, 0))

soft_thresholded_normal_mean <- function(lambda, mu, sigma) {
  mu - lambda - mu * (pnorm(lambda, mu, sigma) - pnorm(-lambda, mu, sigma)) +
    lambda * (pnorm(lambda, mu, sigma) + pnorm(-lambda, mu, sigma)) +
    sigma^2 * (dnorm(lambda, mu, sigma) - dnorm(-lambda, mu, sigma))
}

soft_thresholded_normal_variance <- function(lambda, mu, sigma) {
  2 * mu^2 + 2 * lambda^2 + 2 * sigma^2 -
    ((mu + lambda)^2 + sigma^2) * (1 - pnorm(-lambda, mu, sigma)) - (mu + lambda) * sigma^2 * dnorm(-lambda, mu, sigma) -
    ((mu - lambda)^2 + sigma^2) * (pnorm(lambda, mu, sigma)) + (mu - lambda) * sigma^2 * dnorm(lambda, mu, sigma) - thresholded_normal_mean(lambda, mu, sigma)^2
}

soft_thresholded_mse <- Vectorize(function(lambda, mu, sigma) {
  return((mu - soft_thresholded_normal_mean(lambda, mu, sigma))^2 + soft_thresholded_normal_variance(lambda, mu, sigma))
})

soft_thresholded_optimal_lambda <- function(mu, sigma, lambda_max = 2) {
  #optimize(\(lambda) mse(lambda, mu, sigma), c(0, lambda_max))$minimum
  lambdas <- seq(0, lambda_max, length.out = 1e3)
  mses <- mse(lambdas, mu, sigma)
  lambdas[which.min(mses)]
}
