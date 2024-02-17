#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom methods is
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


utils::globalVariables(c("abline", "aggregate", "coef", "cov", "covar",
                         "dist", "dist_rounded", "id", "legend", "lines",
                         "lm", "mcap", "median", "nls", "rect", "res",
                         "values", "x", "x2", "y", "y2", "z", "z2", "s"))
