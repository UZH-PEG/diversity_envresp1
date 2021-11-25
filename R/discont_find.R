test_function <- function(x) {
  v <- c(rep(1, 10), rep(10, 10))
  v[x]
}



test_function  <- function(x) ifelse(x > -4.5, ifelse(x < 4, exp(-1/abs(x - 1)), 10), 10)
x <- seq(-10, 3, length=100)
plot(x, test_function(x))

return_diff <- function(x, diff_x = 1) {
  test_function(x) - test_function(x + diff_x)
}

optimize(return_diff, interval = c(-10,3), maximum = TRUE)






