hard_threshold <- \(x, lambda) ifelse(x < -lambda, x, ifelse(x > lambda, x, 0))

hard_thresholded_normal_mean <- function(lambda, mu, sigma) {
  truncnorm::etruncnorm(-Inf, -lambda, mu, sigma) * pnorm(-lambda, mu, sigma) +
    truncnorm::etruncnorm(lambda, Inf, mu, sigma) * (1 - pnorm(lambda, mu, sigma))
}

hard_thresholded_normal_variance <- function(lambda, mu, sigma) {
  ex2 <- (truncnorm::vtruncnorm(-Inf, -lambda, mu, sigma) + (truncnorm::etruncnorm(-Inf, -lambda, mu, sigma))^2) * pnorm(-lambda, mu, sigma) +
    (truncnorm::vtruncnorm(lambda, Inf, mu, sigma) + (truncnorm::etruncnorm(lambda, Inf, mu, sigma))^2) * (1 - pnorm(lambda, mu, sigma))
  ex2 - hard_thresholded_normal_mean(lambda, mu, sigma)^2
}

hard_thresholded_mse <- Vectorize(function(lambda, mu, sigma) {
  return(sum((mu - hard_thresholded_normal_mean(lambda, mu, sigma))^2) + sum(hard_thresholded_normal_variance(lambda, mu, sigma)))
}, vectorize.args = "lambda")

hard_thresholded_optimal_lambda <- function(mu, sigma, lambda_max = 2) {
  #optimize(\(lambda) hard_thresholded_mse(lambda, mu, sigma), c(0, lambda_max))$minimum
  lambdas <- seq(0, lambda_max, length.out = 1e3)
  mses <- hard_thresholded_mse(lambdas, mu, sigma)
  lambdas[which.min(mses)]
}
