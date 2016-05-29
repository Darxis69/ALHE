source("TabuSearch.R")

importMeals <- function()
{
  meals <<- read.csv("meals.csv", header = TRUE)
}

initialize_ALHE <- function()
{
  importMeals()
}

initialize_ALHE()

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

result = tabuSearch(10, 0, stopConditionFunc, neighborHoodFunc, evaluateFunc)
print(result)
