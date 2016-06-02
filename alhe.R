source("TabuSearch.R")
source("MealsLogic.R")


initialize_ALHE <- function()
{
  #TODO source('config_vars.r')
  #TODO with(conf, {cat(a)})
  allDishesCount <<- 198
  importDishes(allDishesCount)
  dishesPerMeal <<- 3
  mealsPerDay <<- 5
  optimalCarbohydrates <<- 250
  optimalProteins <<- 200
  optimalFats <<- 80
  carbohydratesPriority <<- 3
  proteinsPriority <<- 5
  fatsPriority <<- 1
  monotonyPriority <<- 1
  objectiveFuncPriority <<- 1
  heuristicFuncPriority <<- 1
  stopCondition <<- 0.995
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
  return(evaluateFunc(point) > stopCondition)
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


#Wykresy pokazac zaleznosc ilosci potraw
#wagi wszystkiego
result = tabuSearch(generateRandomPoint(), stopConditionFunc, neighborHoodFunc, evaluateFunc)
diet <- result[[1]]
observations <- result[[2]]

print(paste("Proteins wanted: ", optimalProteins, ". Got: ", sumDailyProteins(diet), sep = ''))
print(paste("Carbohydrates wanted: ", optimalCarbohydrates, ". Got: ", sumDailyCarbohydrates(diet), sep = ''))
print(paste("Fats wanted: ", optimalFats, ". Got: ", sumDailyFats(diet), sep = ''))
print(paste("Observations: ", observations))
print(diet)
