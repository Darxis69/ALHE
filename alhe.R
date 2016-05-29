source("TabuSearch.R")

stopConditionFunc <- function(point)
{
  #Przerywamy, jeżeli współczynnik dopasowania punktu osiągnie daną wartość
  return(evaluateFunc(point) > 0.9)
}

neighborHoodFunc <- function(point)
{
  #TODO
  return(c(point-0.01, point-0.05, point+0.01, point+0.014, point+0.05))
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
print(meals)

result = tabuSearch(10, 0, stopConditionFunc, neighborHoodFunc, evaluateFunc)
print(result)

