source("TabuSearch.R")

stopConditionFunc <- function(point)
{
  #TODO
  return(point > 30)
}

neighborHoodFunc <- function(point)
{
  #TODO
  return(c(point-10, point-5, point+1, point+14, point+5))
}

evaluateFunc <- function(point)
{
  #TODO
  return(point)
}

importMeals <- function()
{
  read.csv("meals.csv", header = TRUE)
}

meals <- importMeals()

result = tabuSearch(10, 0, stopConditionFunc, neighborHoodFunc, evaluateFunc)

