#'@export

linreg <- function(formula, data) {

  linregObj <- linregClass$new(formula = formula,
                               data = data)
  return(linregObj)
}
