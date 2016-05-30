importDishes <- function()
{
  dishes <<- read.csv("dishes.csv", header = TRUE)
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

sumMeal <- function(meal, selector)
{
  sum <- 0
  for (i in 1:dishesPerMeal)
  {
    sum <- sum + meal[[i]][[selector]]
  }
  
  return(sum)
}

sumDaily <- function(daily, selector)
{
  sum <- 0
  for (i in 1:mealsPerDay)
  {
    sum <- sum + sumMeal(daily[[i]], selector)
  }
  
  return(sum)
}

sumDailyCarbohydrates <- function(point)
{
  return(sumDaily(point, 2))
}

sumDailyProteins <- function(point)
{
  return(sumDaily(point, 3))
}

sumDailyFats <- function(point)
{
  return(sumDaily(point, 4))
}