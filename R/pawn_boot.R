
# Bootstrap Efron
student_ci <- function(x) {
  original <- mean(x)
  ci <- mean(x) + stats::qt(0.975, length(x) - 1) *
    stats::sd(x) * c(-1, +1) / sqrt(length(x))
  high.ci <- ci[[2]]
  low.ci <- ci[[1]]
  return(cbind(original, low.ci, high.ci))
}

#' Computation of confidence intervals
#'
#' @param data The output of the \code{pawn_indices} function.
#' @importFrom data.table ".SD"
#'
#' @return A data table.
#' @export
#'
#' @examples
#' \donttest{# Create matrix:
#' A <- randtoolbox::sobol(n = 1000, dim = 8)
#' # Compute model output:
#' Y <- sobol_Fun(A)
#' Ti <- pawn_generic(data = A, Y = Y, n = 10, test = median, R = 50 )
#' # Compute confidence intervals:
#' pawn_ci(Ti)}
pawn_ci <- function(data) {
  parameters <- colnames(data)
  dt <- data.table::data.table(t(data[, lapply(.SD, student_ci)]))[
    , parameters:= cbind(parameters)
    ]
  dt <- data.table::setcolorder(dt, "parameters")
  dt <- data.table::setnames(dt, c("V1", "V2", "V3"),
                             c("original", "low.ci", "high.ci"))
  return(dt)
}

#------------------------------------------------------------------------------

# Function to compute PAWN of a dummy parameter
pawnD <- function(Y, n) {
  YUi <- sample(1:length(Y), floor(length(Y) / n))
  YU1 <- Y[YUi]
  # This is to avoid ties
  YUj <- Y[-YUi]
  YU2 <- sample(YUj, floor(length(Y) / n))
  ks <- stats::ks.test(YU1, YU2)$statistic[[1]]
  return(ks)
}


#' Computation of the PAWN index of a dummy parameter
#'
#' The function computes and bootstraps the PAWN index of a dummy parameter
#' following the approach by \insertCite{Pianosi2018;textual}{pawnr}.
#'
#' @param Y Numeric vector including the unconditional model output.
#' @param n Integer, number of conditioning intervals.
#' @param R Integer, number of bootstrap replicas.
#'
#' @importFrom Rdpack reprompt
#' @references
#' \insertAllCited{}
#'
#' @return A matrix with the bootstrap replicas.
#' @export
#'
#' @examples
#' # Create matrix:
#' A <- randtoolbox::sobol(n = 1000, dim = 8)
#' # Compute model output:
#' Y <- sobol_Fun(A)
#' # Compute PAWN index of a dummy parameter:
#' dummy <- pawn_dummy(Y, n = 10, R = 10)
pawn_dummy <- function(Y, n, R) {
  # Bootstrap the function
  out <- mcreplicate(R, pawnD(Y, n))
  # Compute mean and quantiles
  original <- mean(out)
  q <- stats::quantile(out, probs = c(0.025, 0.975))
  low.ci <- q[[1]]
  high.ci <- q[[2]]
  return(data.table::data.table(original, low.ci, high.ci))
}

