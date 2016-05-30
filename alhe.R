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
  carbohydratesPriority <<- 1
  proteinsPriority <<- 2
  fatsPriority <<- 3
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
  return(evaluateFunc(point) > 0.995)
}

arePointsIdentical <- function(point1, point2)
{
  if (length(point1) != length(point2))
  {
    return(FALSE)
  }
  for (mealNo in 1:length(point1))
  {
    meal1 <- point1[[mealNo]]
    meal2 <- point2[[mealNo]]
    if (length(meal1) != length(meal2))
    {
      return(FALSE)
    }
    for (dishNo in 1:length(meal1))
    {
      dish1 <- meal1[[dishNo]]
      dish2 <- meal2[[dishNo]]
      if (!identical(dish1[1], dish2[1]))
      {
        return(FALSE)
      }
    }
  }
  return(TRUE)
}

neighborHoodFunc <- function(point)
{
  dishesSize <- nrow(dishes)
  
  neighborHoodSize <- dishesPerMeal*mealsPerDay*(dishesSize-1)
  neighborHoodInsertElementIndex <- 1
  neighborHood <- vector(mode = "list", length = neighborHoodSize)
  
  for (mealNo in 1:mealsPerDay)
  {
    meal = point[[mealNo]]
    for (dishNo in 1:dishesPerMeal)
    {
      dish = meal[[dishNo]]
      for (selectedDishIndex in 1:dishesSize)
      {
        selectedDish <- dishes[selectedDishIndex, ]
        if (!identical(selectedDish[1], dish[1]))
        {
          neighbor <- point
          neighbor[[mealNo]][[dishNo]] <- selectedDish
          neighborHood[[neighborHoodInsertElementIndex]] <- neighbor
          neighborHoodInsertElementIndex <- neighborHoodInsertElementIndex + 1
        }
      }
    }
  }
  
  return(neighborHood)
}

randomPoint <- generateRandomPoint()
neighborHood <- neighborHoodFunc(randomPoint)

evaluateFunc <- function(point)
{
  #Sumujemy priorytety (wagi)
  prioritiesSum <- carbohydratesPriority + proteinsPriority + fatsPriority
  
  #Obliczamy stosunek optymalnej do uzyskanej
  xCarbohydrates <- sumDailyCarbohydrates(point) / optimalCarbohydrates
  
  #Jeżeli większe od 1 to normalizujemy (np. 1.2 przechodzi w 0.8)
  if (xCarbohydrates > 1) xCarbohydrates <- 1 - (xCarbohydrates - 1)
  
  #Mnożymy razy wagę
  xCarbohydrates <- xCarbohydrates * (carbohydratesPriority / prioritiesSum)
  
  #Powtarzamy dla pozostałych makroskładników
  xProteins <- sumDailyProteins(point) / optimalProteins
  if (xProteins > 1) xProteins <- 1 - (xProteins - 1)
  xProteins <- xProteins * (proteinsPriority / prioritiesSum)
  
  xFats <- sumDailyFats(point) / optimalFats
  if (xFats > 1) xFats <- 1 - (xFats - 1)
  xFats <- xFats * (fatsPriority / prioritiesSum)
  
  #Sumujemy całość
  xSum = xCarbohydrates + xProteins + xFats
  
  return(xSum)
}

result = tabuSearch(10, generateRandomPoint(), stopConditionFunc, neighborHoodFunc, evaluateFunc)
print(paste("Carbohydrates wanted: ", optimalCarbohydrates, ". Got: ", sumDailyCarbohydrates(result), sep = ''))
print(paste("Proteins wanted: ", optimalProteins, ". Got: ", sumDailyProteins(result), sep = ''))
print(paste("Fats wanted: ", optimalFats, ". Got: ", sumDailyFats(result), sep = ''))
