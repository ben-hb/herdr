#' Generate a Matrix with One Unit for each Agent
#'
#' This function generates a numeric matrix with one row for iteration and one
#' column for each generation. It assumes that each iteration has an identical
#' number of generations and is filled with 0s as placeholders.
#'
#' @param iterations Number of iterations
#' @param generations Number of generations
#' @return A numeric matrix filled with 0s
#' @export

init_matrix <- function(iterations, generations) {
  temp_matrix <- matrix(0, iterations, generations)
  return(temp_matrix)
}

# Loop over state

gen_signals <- function(signal_matrix, eta, alpha, beta, state) {
  signals <- sample(c("a", "b", "o"), size = length(signal_matrix), replace = TRUE,
                    # P(signal = b | state = A) = 1 - P(signal = 0 | state = A) - P(signal = a | state = A)
                    prob = c(ifelse(state == "a", alpha * (1 - eta), 1 - eta - beta * (1 - eta)),
                             # P(signal = a | state = B) = 1 - P(signal = o | state = B) - P(signal = b | state = B)
                             ifelse(state == "b", beta * (1 - eta), 1 - eta - alpha * (1 - eta)),
                             eta))
  return(matrix(signals, nrow(signal_matrix), ncol(signal_matrix)))
}
