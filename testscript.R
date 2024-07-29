library(testthat)
context("Testing Shapiro-Wilk test")

## Invalid inputs testing
test_that("Our script gives helpful errors for invalid inputs", {
  
  expect_error(my_shapiro_wilk_test(c(303,NA,406,457,461,469,474,489,515,583),TRUE),"There seems to be at least one NA value in the data.")
  
  expect_error(my_shapiro_wilk_test(c(303,Inf,406,457,461,469,474,489,515,583),TRUE),"There seems to be at least one infinity value in the data.")
  
  expect_error(my_shapiro_wilk_test(matrix(c(303,338,406,457,461,469,474,489,515,583), nrow=2, ncol=5),TRUE),"There seems to be a problem with the dimensions of the data.")
  
  expect_error(my_shapiro_wilk_test(matrix(c(303,338,406,457,461,469,474,489,515,583), nrow=2, ncol=5),TRUE),"There seems to be a problem with the dimensions of the data.")
  
  expect_error(my_shapiro_wilk_test(c(303,338),TRUE),"The data does not have enough elements.")
  
  expect_error(my_shapiro_wilk_test(rnorm(2001),TRUE),"The data has too many elements.")
  
})
## Valid inputs testing
test_that("Test if the script work with or without the approx argument", {
  expect_silent(my_shapiro_wilk_test(c(303,338,406,457,461,469,474,489,515,583)))  
  expect_silent(my_shapiro_wilk_test(c(303,338,406,457,461,469,474,489,515,583),TRUE))
  expect_silent(my_shapiro_wilk_test(c(303,338,406,457,461,469,474,489,515,583),FALSE))
})  


## Functionality testing (not much to do here.)

test_that("Test if script finds approximate the right values", {
  
  # Example 1: (Sapiro-Wilk paper, page 16, W=0.9530)
  expect_equal(my_shapiro_wilk_test(c(6,1,-4,8,-2,5,0)),0.953,tolerance=0.02)
  expect_equal(my_shapiro_wilk_test(c(6,1,-4,8,-2,5,0),FALSE),0.953,tolerance=0.02)
  
  # Example 2: (Sapiro-Wilk paper, page 16, W=0.79)    
  expect_equal(my_shapiro_wilk_test(c(148,154,158,160,161,162,166,170,182,195,236)),0.79,tolerance=0.02)
  expect_equal(my_shapiro_wilk_test(c(148,154,158,160,161,162,166,170,182,195,236),FALSE),0.79,tolerance=0.02)  
  
  # Example 3: (Sapiro-Wilk paper, page 16, W=0.943)    
  expect_equal(my_shapiro_wilk_test(c(303,338,406,457,461,469,474,489,515,583)),0.943,tolerance=0.02)
  expect_equal(my_shapiro_wilk_test(c(303,338,406,457,461,469,474,489,515,583),FALSE),0.943,tolerance=0.02)
  
})
