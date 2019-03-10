

#' Plot PAWN indices
#'
#' @param data The output of the \code{pawn_ci} function.
#'
#' @return A ggplot object
#' @import ggplot2
#' @export
#'
#' @examples
#' \donttest{# Create matrix:
#' A <- randtoolbox::sobol(n = 1000, dim = 8)
#' # Compute model output:
#' Y <- sobol_Fun(A)
#' T <- pawn_generic(data = A, Y = Y, n = 10, test = median, R = 50 )
#' T.ci <- pawn_ci(T)
#' plot_pawn(T.ci)}
plot_pawn <- function(data) {
  low.ci <- high.ci <- parameters <- original <- NULL
  ggplot2::ggplot(data, aes(parameters, original)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = low.ci,
                    ymax = high.ci)) +
  labs(x = "",
       y = expression(T[i])) +
  scale_fill_discrete(guide = FALSE) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill = "transparent",
                                         color = NA),
        legend.key = element_rect(fill = "transparent",
                                  color = NA))
}
