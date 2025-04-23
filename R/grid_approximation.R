# Module to generate grid approximation figures

# Packages
library(ggplot2)
library(tibble)

# Generate grid
get_p_grid <- function(grid.length) {
  output <- seq(
    from = 0,
    to = 1,
    length.out = grid.length
  )

  return(output)
}

# Obtain different priors
get_uniform_prior <- function(prob) {
  output <- rep(1, length(prob))

  return(output)
}

get_truncated_prior <- function(prob) {
  output <- ifelse(prob < 0.5, 0, 1)

  return(output)
}

get_peaked_prior <- function(prob) {
  output <- exp(-5 * abs(prob - 0.5))

  return(output)
}

# Compute binomial likelihood
compute_binom_likelihood <- function(obs, size, prob) {
  output <- dbinom(
    x = obs,
    size = size,
    prob = prob
  )

  return(output)
}

# Compute unstandardized posterior
compute_unst_posterior <- function(likelihood, prior) {
  output <- likelihood * prior

  return(output)
}

# Compute standardized posterior
compute_st_posterior <- function(unst.posterior) {
  output <- unst.posterior / sum(unst.posterior)

  return(output)
}

# Plot posterior
plot_grid_posterior <- function(
  prob,
  st.posterior,
  prior
) {
  # Input data
  input_tbl <- tibble::tibble(
    grid_prob = prob,
    st_posterior = st.posterior
  )

  # Plot
  output <- ggplot2::ggplot() +

    # Points
    ggplot2::geom_point(
      data = input_tbl,
      ggplot2::aes(
        x = grid_prob,
        y = st_posterior
      )
    ) +

    # Lines
    ggplot2::geom_line(
      data = input_tbl,
      ggplot2::aes(
        x = grid_prob,
        y = st_posterior
      )
    ) +

    # Titles
    ggplot2::labs(
      title = paste0(nrow(input_tbl), " points, ", tolower(prior), " prior"),
      x = "Probability of water",
      y = "Posterior probability"
    ) +

    # Theme
    ggplot2::theme_bw()
  
  # Return
  return(output)
}

# Run grid approximation
run_grid_approximation <- function(
  grid.length,
  prior,
  obs,
  size
) {
  # Grid
  p_grid <- get_p_grid(grid.length = grid.length)

  # Select prior
  if (prior == "Uniform") {
    prior_prob <- get_uniform_prior(prob = p_grid)

  } else if (prior == "Truncated") {
    prior_prob <- get_truncated_prior(prob = p_grid)

  } else if (prior == "Peaked") {
    prior_prob <- get_peaked_prior(prob = p_grid)
  }

  # Compute likelihood
  likelihood <- compute_binom_likelihood(
    obs = 6,
    size = 9,
    prob = p_grid
  )

  # Compute posterior
  unst_post <- compute_unst_posterior(
    likelihood = likelihood,
    prior = prior_prob
  )

  st_post <- compute_st_posterior(unst_post)

  # Plot posterior
  grid_approx_fig <- plot_grid_posterior(
    prob = p_grid,
    st.posterior = st_post,
    prior = prior
  )
  
  # Return
  return(grid_approx_fig)
}

# UI module
gridApproxUI <- function(id) {
  tagList(
    numericInput(
      NS(id, "grid_length"),
      "Grid length",
      min = 5,
      max = 1000,
      step = 1,
      value = 5
    ),

    textOutput(NS(id, "grid_points"))
    # plotOutput("grid_approx_fig")
  )
}
