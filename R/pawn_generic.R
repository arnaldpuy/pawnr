
# DIVIDE A VECTOR INTO N CHUNKS -----------------------------------------------

chunks <- function(x, n) {
    out <- split(x, ceiling(seq_along(x) / (length(x) / n)))
    return(out)
}

# PARALLEL VERSION OF REPLICATE -----------------------------------------------

mcreplicate <- function(n, expr, simplify = "array",...) {
  answer <-
    parallel::mclapply(integer(n), eval.parent(substitute(function(...) expr)),...)
  if (!identical(simplify, FALSE) && length(answer))
    return(simplify2array(answer, higher = (simplify == "array")))
  else return(answer)
}

# PAWN INDEX (GENERIC) --------------------------------------------------------

pawnG <- function(data, Y, n, test) {
  ks <- parameters <- value <- V1 <- NULL
  index <- sample(1:nrow(data),
                  size = floor(nrow(data) / n),
                  # Without replacement
                  replace = FALSE)
  # Bind model inputs and model output
  dt <- data.table::data.table(cbind(data, Y))
  # Subset and obtain the unconditional model output
  Y_unc <- dt[index, Y]
  # Create the intervals
  melted <- data.table::melt(dt,
                             measure.vars = 1:(ncol(dt) - 1),
                             variable.name = "parameters")
  out <- melted[order(parameters, value)][
    , list(chunks(Y, n)), parameters][
    , ID:= .I][
    , Y_unc:= replicate(n * ncol(data), Y_unc, simplify = FALSE)][
    , ks:= mapply(stats::ks.test, Y_unc, V1), list(parameters, ID)][
    , test(ks), parameters][
    , V1
    ]
  return(out)
}

# BOOTSTRAP PAWN INDEX (GENERIC) ----------------------------------------------

#' Computation of PAWN indices (generic approach)
#'
#' This function computes and bootstraps PAWN indices following the
#' "generic approach" of \insertCite{Pianosi2018;textual}{pawnr}.
#'
#' @param data A data frame or matrix with the sample matrix.
#' @param Y Numeric vector, model output.
#' @param n Integer, number of conditioning intervals.
#' @param test Summary statistic (i.e. \code{mean}, \code{median}, \code{max}).
#' @param R Integer, number of bootstrap replicas.
#'
#' @importFrom Rdpack reprompt
#' @importFrom rlang ":="
#' @importFrom data.table ".I"
#' @details The bootstrap is conducted without replacement, as recommended by
#' \insertCite{KhorashadiZadeh2017;textual}{pawnr}.
#' @references
#' \insertAllCited{}
#'
#' @return A data table with the bootstrap PAWN indices of each model input.
#' @export
#'
#' @examples
#' \donttest{# Create matrix:
#' A <- randtoolbox::sobol(n = 100, dim = 8)
#' # Compute model output:
#' Y <- sobol_Fun(A)
#' T <- pawn_generic(data = A, Y = Y, n = 10, test = median, R = 50 )}
pawn_generic <- function(data, Y, n, test, R) {
  dt <- data.table::data.table(t(mcreplicate(R, pawnG(data = data,
                                                      Y = Y,
                                                      n = n,
                                                      test = test))))
  return(dt)
}


