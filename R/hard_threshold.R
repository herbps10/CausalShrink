hard_threshold <- \(x, lambda) ifelse(x < -lambda, x, ifelse(x > lambda, x, 0))

hard_thresholded_normal_mean <- function(lambda, mu, sigma) {
  integrate(\(x) x * dnorm(x, mean = mu, sd = sigma), -Inf, -lambda)$value +
    integrate(\(x) x * dnorm(x, mean = mu, sd = sigma), lambda, Inf)$value
}

hard_thresholded_normal_variance <- function(lambda, mu, sigma) {
  mu2 <- integrate(\(x) x^2 * dnorm(x, mean = mu, sd = sigma), -Inf, -lambda)$value +
    integrate(\(x) x^2 * dnorm(x, mean = mu, sd = sigma), lambda, Inf)$value
  mu2 - hard_thresholded_normal_mean(lambda, mu, sigma)^2
}

hard_thresholded_mse <- Vectorize(function(lambda, mu, sigma) {
  return((mu - hard_thresholded_normal_mean(lambda, mu, sigma))^2 + hard_thresholded_normal_variance(lambda, mu, sigma))
})

hard_thresholded_optimal_lambda <- function(mu, sigma, lambda_max = 2) {
  #optimize(\(lambda) mse(lambda, mu, sigma), c(0, lambda_max))$minimum
  lambdas <- seq(0, lambda_max, length.out = 1e3)
  mses <- hard_thresholded_mse(lambdas, mu, sigma)
  lambdas[which.min(mses)]
}
