#' Custom pipe operator for compatibility
#'
#' Provides a pipe operator that mimics the native pipe operator
#' for compatibility with older R versions.
#'
#' @name |>
#' @keywords internal
#' @usage lhs |> rhs
#' @param lhs A value to be passed to the function.
#' @param rhs A function call where lhs is the first argument.
#' @return The result of calling `rhs(lhs)`.
NULL
# Define the custom pipe operator for older R versions
if (getRversion() < "4.1.0") {
  `|>` <- function(lhs, rhs) {
    # Get the right-hand side expression without evaluating it
    rhs_call <- substitute(rhs)

    if (is.call(rhs_call)) {
      # Reconstruct the call:
      # - The first element is the function name (e.g., sum)
      # - The second element is the new first argument (lhs)
      # - The rest are the original arguments from the rhs call
      full_call <- as.call(c(rhs_call[[1]], lhs, as.list(rhs_call[-1])))
      eval(full_call, parent.frame())
    } else {
      # Handle cases like x |> function, which should just be the function
      (rhs)(lhs)
    }
  }
}
