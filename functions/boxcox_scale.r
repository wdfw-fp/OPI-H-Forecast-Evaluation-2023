boxcox_scale <- function(x) {
  if (any(x < 0)) {
    # shift x by adding the absolute value of the minimum value plus one
    x <- x + abs(min(x)) + 1
  }
  # fit a linear model with x as the response variable
  model <- lm(x ~ 1)
  # find the optimal lambda for box-cox transformation
  bc <- boxcox(model)
  lambda <- bc$x[which.max(bc$y)]
  # apply the box-cox transformation with the optimal lambda
  y <- (x^lambda - 1) / lambda
  # scale the transformed vector to have mean 0 and standard deviation 1
  z <- scale(y)
  # return the scaled vector
  return(z)
}