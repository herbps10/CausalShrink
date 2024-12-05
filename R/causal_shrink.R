causal_shrink <- function(point_estimates, efficiency_bounds, sample_size, method = "l2", lambda_max = 2) {
  if(method == "l2") {
    lambda <- (efficiency_bounds / sample_size) / (point_estimates^2)
    shrinkage <- (1 / (1 + lambda))

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

  point_estimates_star
}
