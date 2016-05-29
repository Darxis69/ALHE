source("TabuSearch.R")

importMeals <- function()
{
  meals <<- read.csv("meals.csv", header = TRUE)
}

initialize_ALHE <- function()
{
  importMeals()
  mealsPerDay <<- 5
}

initialize_ALHE()

generateRandomPoint <- function()
{
  randomPoint <- list()
  mealsSize <- nrow(meals)
  for (mealNo in 1:mealsPerDay)
  {
    randomMealIndex <- sample(1:mealsSize, 1)
    randomMeal <- meals[randomMealIndex, ]
    randomPoint <- append(randomPoint, list(randomMeal))
  }
  
  return(randomPoint)
}

stopConditionFunc <- function(point)
{
  #Przerywamy, jeżeli współczynnik dopasowania punktu osiągnie daną wartość
  return(evaluateFunc(point) > 0.9)
}

neighborHoodFunc <- function(point)
{
  mealsSize <- nrow(meals)
  
  neighborHoodSize <- mealsPerDay*(mealsSize-1)
  neighborHoodElementsAdded <- 0
  neighborHood <- vector(mode = "list", length = neighborHoodSize)
  
  for (mealNo in 1:mealsPerDay)
  {
    for (selectedMealIndex in 1:mealsSize)
    {
      selectedMeal <- meals[selectedMealIndex, ]
      if (!identical(selectedMeal["MealName"], point[[mealNo]]["MealName"]))
      {
        neighbor <- point
        neighbor[mealNo] <- list(selectedMeal)
        neighborHood[neighborHoodElementsAdded] <- neighbor
        neighborHoodElementsAdded <- neighborHoodElementsAdded + 1
      }
    }
  }
  
  return(neighborHood)
}

randomPoint <- generateRandomPoint()
neighborHood <- neighborHoodFunc(randomPoint)

evaluateFunc <- function(point)
{
  #TODO
  return(1)
}

#result = tabuSearch(10, generateRandomPoint(), stopConditionFunc, neighborHoodFunc, evaluateFunc)
#print(result)
