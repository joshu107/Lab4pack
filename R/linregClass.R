linregClass <- setRefClass(

  Class = "linregClass",

  fields = list(
    formula = "formula",
    data = "data.frame"
  ),

  methods = list(

    extract = function(formula, data) {
      #Form X (model)matrix and Y vector out of data using formula
      x <<- model.matrix(object = formula,
                        data = data)
      y <<- data[, all.vars(formula)[1]]
      res <- list(X = x,
                y = y)
      return(res)
    },

    coef = function() {
      #extract data for use
      xy <- extract(formula, data)
      x <- xy$X
      y <- xy$y

      #Calculate regression coefficients
      betahat <- solve(t(x) %*% x) %*% t(x) %*% y
      #format like lm$coefficients output
      betahat <- t(betahat)
      return(betahat)
    },

    pred = function() {
      #extract data for use
      xy <- extract(formula, data)
      x <- xy$X
      betahat <- coef()

      #Calculate the fitted values
      yhat <- x %*% t(betahat)
      return(yhat)
    },

    resid = function() {
      #extract data for use
      xy <- extract(formula, data)
      y <- xy$y
      yhat <- pred()

      #Calculate residuals
      ehat <- y - yhat
      return(ehat)
    },

    print = function(digits = 3, ...) {
      betahat <- coef()

      res <- cat(paste0("Call:\nlinreg(formula =",
                   formula, ", data =  iris",
                   ")\n\nCoefficients :\n", betahat))
      print(res)

    }
  )
)
