
# CREATION OF THE PAWN SCRAMBLED MATRIX ---------------------------------------

scrambled_pawn <- function(A, Nc, n) {
    # Select the conditional values
    n_cond <- apply(A, 2, function(x) seq(min(x),
                                          max(x),
                                          length.out = n))
    X <- A
    for(i in 1:ncol(A)) {
        for (j in 1:nrow(n_cond)) {
            # Generate Nc random samples of the original matrix
            AB <- A[sample(nrow(A), Nc, replace = TRUE), ]
            # Fill in with the conditioning value of X~i
            AB[, i] <- n_cond[j, i]
            X <- rbind(X, AB)
        }
    }
    return(X)
}

# CREATION OF THE MATRICES TO COMPUTE PAWN ------------------------------------

#' Creation of the matrices to compute PAWN (tailored approach)
#'
#' Creates the matrices to compute the model output and then calculate
#' the PAWN sensitivity indices of the model inputs following the
#' 'tailored approach'. The matrix for the unconditional model output
#' is created via Sobol' quasi-random number sequences.
#'
#' @param N Numeric, sample size of the unconditional model output
#' @param Nc Numeric, sample size of the conditional model output
#' @param n Numeric, number of conditioning intervals
#' @param k Numeric, number of parameters
#'
#' @return A matrix
#' @export
#'
#' @examples
#' pawn_matrix(N = 10,
#' Nc = 5,
#' n = 3,
#' k = 3)
pawn_matrix <- function(N, Nc, n, k) {
    if (Nc > N)
        stop("Nc must be smaller or equal than N")
    # Create the Sobol quasi-random number sequence
    A <- randtoolbox::sobol(n = N, dim = k)
    # Create AB matrix
    AB <- scrambled_pawn(A, Nc, n)
    return(AB)
}
