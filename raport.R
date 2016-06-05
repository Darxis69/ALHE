source("alhe.R")

generateData <-function(n, mode)
{
  result <- vector(mode = "list", length = n)
  for(i in 1:n)
  {
    result[[i]] <- executeWithConfig(configVariablesToTest[[mode]])
  }
  
  return(result)
}

showPlot <- function(input, selector, plotTitle)
{
  output <- list()
  for(i in 1:length(input))
  {
    output[[i]] <- input[[i]][[selector]]
  }
  
  outputList <- unlist(output)
  
  plot(outputList) 
  if (selector == 3)
  {
    abline(h = optimalProteins, v = 0, col = "red")
  }
  if (selector == 4)
  {
    abline(h = optimalCarbohydrates, v = 0, col = "red")
  }
  if (selector == 5)
  {
    abline(h = optimalFats, v = 0, col = "red")
  }
  title(paste(plotTitle))
}

showPlots <- function(diet, plotTitleSuffix)
{
  showPlot(diet, 3, paste("Proteins", plotTitleSuffix))
  showPlot(diet, 4, paste("Carbohydrates", plotTitleSuffix))
  showPlot(diet, 5, paste("Fats", plotTitleSuffix))
  showPlot(diet, 6, paste("Duplicate", plotTitleSuffix))
}

diet1 <- generateData(10, 1)
showPlots(diet1, "(standard)")
diet2 <- generateData(10, 2)
showPlots(diet2, "(carbohydrates priority = 3)")
diet3 <- generateData(10, 3)
showPlots(diet3, "(proteins priority = 3)")
diet4 <- generateData(10, 4)
showPlots(diet4, "(fats priority = 3)")
diet5 <- generateData(10, 5)
showPlots(diet5, "(monotony ratio = 2)")
diet6 <- generateData(10, 6)
showPlots(diet6, "(monotony ratio = 0.5)")
diet7 <- generateData(10, 7)
showPlots(diet7, "(3 meals, 3 dishes)")
diet8 <- generateData(10, 8)
showPlots(diet8, "(5 meals, 3 dishes)")
diet9 <- generateData(10, 9)
showPlots(diet9, "(4 meals, 2 dishes)")
diet10 <- generateData(10, 10)
showPlots(diet10, "(4 meals, 4 dishes)")
diet11 <- generateData(10, 11)
showPlots(diet11, "(only first 50 dishes from database)")
diet12 <- generateData(10, 12)
showPlots(diet12, "(only first 120 dishes from database)")
diet13 <- generateData(10, 13)
showPlots(diet13, "(stop condition = 0.5)")
diet14 <- generateData(10, 14)
showPlots(diet14, "(stop condition = 0.75)")
