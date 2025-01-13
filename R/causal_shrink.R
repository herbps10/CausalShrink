causal_shrink <- function(point_estimates, efficiency_bounds, sample_size, method = "l2", type = "default", lambda_max = 2) {
  shrinkage <- rep(NA, length(point_estimates))
  if(method == "l2") {
    lambda <- 1 / sample_size * (sum(efficiency_bounds) / sum(point_estimates^2))
    shrinkage <- rep((1 / (1 + lambda)), length(point_estimates))
    point_estimates_star <- shrinkage * point_estimates
  }
  else if(method == "empirical_bayes") {
    #lambda <- (1 / sample_size) * efficiency_bounds / (1 / length(point_estimates - 1) * sum(point_estimates^2))
    #shrinkage <- 1 / (1 + lambda)
    tau_sq <- 1 / (length(point_estimates) - 1) * sum(point_estimates^2)
    sigma_sq <- 1 / sample_size * efficiency_bounds
    shrinkage <- tau_sq / (sigma_sq + tau_sq)
    point_estimates_star <- shrinkage * point_estimates
  }
  else if(method == "l1") {
    lambda <- soft_thresholded_optimal_lambda(point_estimates, sqrt(efficiency_bounds / sample_size), lambda_max)
    point_estimates_star <- soft_threshold(point_estimates, lambda)
  }
  else if(method == "l0") {
    lambda <- hard_thresholded_optimal_lambda(point_estimates, sqrt(efficiency_bounds / sample_size), lambda_max)
    point_estimates_star <- hard_threshold(point_estimates, lambda)
  }

  list(
    point_estimates = point_estimates_star,
    shrinkage = shrinkage,
    lambda = lambda
  )
}
