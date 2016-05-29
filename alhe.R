source("TabuSearch.R")

importMeals <- function()
{
  meals <<- read.csv("meals.csv", header = TRUE)
}

initialize_ALHE <- function()
{
  importMeals()
  dishesPerMeal <<- 3
  mealsPerDay <<- 5
  optimalCarbohydrates <<- 250
  optimalProteins <<- 200
  optimalFats <<- 80
}

initialize_ALHE()

sumBySelector <- function(point, selector, count)
{
  sum <- 0
  for (i in 1:count)
  {
    sum <- sum + point[[i]][selector]
  }
  
  return(sum)
}

sumCarbohydrates <- function(point)
{
  return(sumBySelector(point, "Carbohydrates", mealsPerDay))
}

sumProteins <- function(point)
{
  return(sumBySelector(point, "Proteins", mealsPerDay))
}

sumFats <- function(point)
{
  return(sumBySelector(point, "Fats", mealsPerDay))
}

generateRandomPoint <- function()
{
  randomPoint <- vector(mode = "list", length = mealsPerDay)
  mealsSize <- nrow(meals)
  for (mealNo in 1:mealsPerDay)
  {
    randomMeal <- vector(mode = "list", length = dishesPerMeal)
    for (dishNo in 1:dishesPerMeal)
    {
      randomDishIndex <- sample(1:mealsSize, 1)
      randomDish <- meals[randomDishIndex, ]
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
  mealsSize <- nrow(meals)
  
  neighborHoodSize <- mealsPerDay*(mealsSize-1)
  neighborHoodInsertElementIndex <- 1
  neighborHood <- vector(mode = "list", length = neighborHoodSize)
  
  for (mealNo in 1:mealsPerDay)
  {
    for (selectedMealIndex in 1:mealsSize)
    {
      selectedMeal <- meals[selectedMealIndex, ]
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
  deltaCarbohydrates <- abs(optimalCarbohydrates - sumCarbohydrates(point))
  deltaProteins <- abs(optimalProteins - sumProteins(point))
  deltaFats <- abs(optimalFats - sumFats(point))
  deltaSum <- deltaCarbohydrates + deltaProteins + deltaFats
  
  if (deltaSum == 0)
  {
    return(1)
  }
  
  return(1/((deltaCarbohydrates + deltaProteins + deltaFats)/100))
}

result = tabuSearch(10, generateRandomPoint(), stopConditionFunc, neighborHoodFunc, evaluateFunc)
print(paste("Carbohydrates wanted: ", optimalCarbohydrates, ". Got: ", sumCarbohydrates(result), sep = ''))
print(paste("Proteins wanted: ", optimalProteins, ". Got: ", sumProteins(result), sep = ''))
print(paste("Fats wanted: ", optimalFats, ". Got: ", sumFats(result), sep = ''))
