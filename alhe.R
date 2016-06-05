library(foreach)
source("TabuSearch.R")
source("MealsLogic.R")
source("ConfigVariables.R")

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
  return(evaluateFunc(point) > stopCondition)
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

monotonyRatio <- function(point)
{
  dishesList <- list()
  index <- 1
  for (mealNo in 1:length(point))
  {
    meal <- point[[mealNo]]
    for (dishNo in 1:length(meal))
    {
      dish <- meal[[dishNo]]
      dishesList[[index]] <- dish[[1]]
      index <- index + 1
    }
  }
  
  duplicates <- duplicated(dishesList)
  occurNo <- 0
  
  for (i in 1:length(duplicates))
  {
    if (duplicates[[i]] == TRUE)
    {
      occurNo <- occurNo + 1
    }
  }
  
  return(occurNo)
}

objectiveFunc <- function(point)
{
  #Sumujemy priorytety (wagi)
  prioritiesSum <- carbohydratesPriority + proteinsPriority + fatsPriority
  
  #Obliczamy stosunek uzyskanej do optymalnej
  xCarbohydrates <- sumDailyCarbohydrates(point) / optimalCarbohydrates
  
  #Jeżeli większe od 1 to normalizujemy (np. 1.2 przechodzi w 0.8)
  if (xCarbohydrates > 1) xCarbohydrates <- 1 - (xCarbohydrates - 1)
  #Jeżeli mniejsze to zmniejszamy dodatkowo tą wagę, bo nie chcemy mieć mniej niż jest w diecie
  else xCarbohydrates <- xCarbohydrates * xCarbohydrates
  
  #Powtarzamy dla pozostałych makroskładników
  xProteins <- sumDailyProteins(point) / optimalProteins
  if (xProteins > 1) xProteins <- 1 - (xProteins - 1)
  else xProteins <- xProteins * xProteins
  
  xFats <- sumDailyFats(point) / optimalFats
  if (xFats > 1) xFats <- 1 - (xFats - 1)
  else xFats <- xFats * xFats
  
  dishesCount <- dishesPerMeal * mealsPerDay
  xMonotonyRatio <- monotonyRatio(point) / dishesCount
  xMonotonyRatio <- xMonotonyRatio * monotonyPriority
  
  #Liczymy średnia ważoną
  xSum = ((xCarbohydrates*carbohydratesPriority)+(xProteins * proteinsPriority)+(xFats * fatsPriority))/prioritiesSum
  
  return(xSum-xMonotonyRatio)
}

heuristicFunc <- function(point)
{
  #Obliczamy stosunek uzyskanej do optymalnej
  xCarbohydrates <- sumDailyCarbohydrates(point) / optimalCarbohydrates
  
  #Jeżeli większe od 1 to normalizujemy (np. 1.2 przechodzi w 0.8)
  if (xCarbohydrates > 1) xCarbohydrates <- 1 - (xCarbohydrates - 1)
  
  #Powtarzamy dla pozostałych makroskładników
  xProteins <- sumDailyProteins(point) / optimalProteins
  if (xProteins > 1) xProteins <- 1 - (xProteins - 1)
  
  xFats <- sumDailyFats(point) / optimalFats
  if (xFats > 1) xFats <- 1 - (xFats - 1)
  
  #Liczymy średnią
  xSum = (xCarbohydrates+xProteins+xFats)/3
  
  return(xSum)
}

evaluateFunc <- function(point)
{
  objectiveFuncValue <- objectiveFunc(point)
  heuristicFuncValue <- heuristicFunc(point)
  
  prioritiesSum <- objectiveFuncPriority + heuristicFuncPriority
  
  return((objectiveFuncValue*objectiveFuncPriority+heuristicFuncValue*heuristicFuncPriority)/prioritiesSum)
}

loadConfigVariablesAsGlobals <- function(configVariables)
{
  foreach(key=names(configVariables), val=configVariables, .combine=rbind, .packages="foreach") %do% assign(key, val, envir = .GlobalEnv)
}

loadDishesAsGlobals <- function()
{
  dishes = importDishes(allDishesCount)
  assign("dishes", dishes, envir = .GlobalEnv)
}

executeWithConfig <- function(configVariablesName)
{
  loadConfigVariablesAsGlobals(configVariablesName)
  loadDishesAsGlobals()
  
  randomPoint <- generateRandomPoint()
  neighborHood <- neighborHoodFunc(randomPoint)
  result = tabuSearch(generateRandomPoint(), stopConditionFunc, neighborHoodFunc, evaluateFunc)
  diet <- result[[1]]
  observations <- result[[2]]
  
  return(list(diet,observations,sumDailyProteins(diet),sumDailyCarbohydrates(diet),sumDailyFats(diet), monotonyRatio(diet)))
}
