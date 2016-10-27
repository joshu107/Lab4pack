#Test cases for checking with lm() NOT TEST SUITE!!

data(iris)

mod_obj <- lm(Petal.Length~Petal.Width, iris)
print(mod_obj)

test_obj <- linreg(Petal.Length~Petal.Width, iris)

test_obj$coef()

test_obj$pred()

test_obj$resid()
