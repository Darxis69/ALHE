importDishes <- function()
{
  dishes <<- read.csv("dishes.csv", header = TRUE)
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

sumDailyProteins <- function(point)
{
  return(sumDaily(point, 2))
}

sumDailyCarbohydrates <- function(point)
{
  return(sumDaily(point, 3))
}

sumDailyFats <- function(point)
{
  return(sumDaily(point, 4))
}