getConfigVariablesToTest <- function()
{
  list(
    list(
      allDishesCount = 50,
      dishesPerMeal = 3,
      mealsPerDay = 5,
      optimalCarbohydrates = 200,
      optimalProteins = 200,
      optimalFats = 100,
      carbohydratesPriority = 3,
      proteinsPriority = 5,
      fatsPriority = 1,
      monotonyPriority = 1,
      objectiveFuncPriority = 1,
      heuristicFuncPriority = 1,
      stopCondition = 0.95
    ),
    list(
      allDishesCount = 198,
      dishesPerMeal = 3,
      mealsPerDay = 5,
      optimalCarbohydrates = 250,
      optimalProteins = 200,
      optimalFats = 80,
      carbohydratesPriority = 3,
      proteinsPriority = 5,
      fatsPriority = 1,
      monotonyPriority = 1,
      objectiveFuncPriority = 1,
      heuristicFuncPriority = 1,
      stopCondition = 0.995
    )
  )
}
