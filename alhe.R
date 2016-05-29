source("TabuSearch.R")
source("MealsLogic.R")


initialize_ALHE <- function()
{
  importDishes()
  dishesPerMeal <<- 3
  mealsPerDay <<- 5
  optimalCarbohydrates <<- 250
  optimalProteins <<- 200
  optimalFats <<- 80
}

initialize_ALHE()

generateRandomPoint <- function()
{
  randomPoint <- vector(mode = "list", length = mealsPerDay)
  mealsSize <- nrow(dishes)
  for (mealNo in 1:mealsPerDay)
  {
    randomMeal <- vector(mode = "list", length = dishesPerMeal)
    for (dishNo in 1:dishesPerMeal)
    {
      randomDishIndex <- sample(1:mealsSize, 1)
      randomDish <- dishes[randomDishIndex, ]
      randomMeal[[dishNo]] <- randomDish
    }
    randomPoint[[mealNo]] <- randomMeal
  }
  
  return(randomPoint)
}

stopConditionFunc <- function(point)
{
  #Przerywamy, jeżeli współczynnik dopasowania punktu osiągnie daną wartość
  return(evaluateFunc(point) > 0.8)
}

neighborHoodFunc <- function(point)
{
  mealsSize <- nrow(dishes)
  
  neighborHoodSize <- mealsPerDay*(mealsSize-1)
  neighborHoodInsertElementIndex <- 1
  neighborHood <- vector(mode = "list", length = neighborHoodSize)
  
  for (mealNo in 1:mealsPerDay)
  {
    for (selectedMealIndex in 1:mealsSize)
    {
      selectedMeal <- dishes[selectedMealIndex, ]
      if (!identical(selectedMeal["MealName"], point[[mealNo]]["MealName"]))
      {
        neighbor <- point
        neighbor[[mealNo]] <- selectedMeal
        neighborHood[[neighborHoodInsertElementIndex]] <- neighbor
        neighborHoodInsertElementIndex <- neighborHoodInsertElementIndex + 1
      }
    }
  }
  
  return(neighborHood)
}

randomPoint <- generateRandomPoint()
neighborHood <- neighborHoodFunc(randomPoint)

evaluateFunc <- function(point)
{
  deltaCarbohydrates <- abs(optimalCarbohydrates - sumDailyCarbohydrates(point))
  deltaProteins <- abs(optimalProteins - sumDailyProteins(point))
  deltaFats <- abs(optimalFats - sumDailyFats(point))
  deltaSum <- deltaCarbohydrates + deltaProteins + deltaFats
  
  if (deltaSum == 0)
  {
    return(1)
  }
  
  return(1/((deltaCarbohydrates + deltaProteins + deltaFats)/100))
}

result = tabuSearch(10, generateRandomPoint(), stopConditionFunc, neighborHoodFunc, evaluateFunc)
print(paste("Carbohydrates wanted: ", optimalCarbohydrates, ". Got: ", sumDailyCarbohydrates(result), sep = ''))
print(paste("Proteins wanted: ", optimalProteins, ". Got: ", sumDailyProteins(result), sep = ''))
print(paste("Fats wanted: ", optimalFats, ". Got: ", sumDailyFats(result), sep = ''))
