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

# generates n results from configVariablesToTest[[mode]]
diet50 <- generateData(10, 2)
diet198 <- generateData(10, 2)

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

# TODO ??? displaying whole diet, is it needed?
# showPlot(diet, 1, "Observations (198 dishes)")

# shows plot based on 1st argument (result), uses selector and title
showPlot(diet50, 2, "Observations (50 dishes)")
showPlot(diet50, 3, "Proteins (50 dishes)")
showPlot(diet50, 4, "Carbohydrates (50 dishes)")
showPlot(diet50, 5, "Fats (50 dishes)")

showPlot(diet198, 2, "Observations (198 dishes)")
showPlot(diet198, 3, "Proteins (198 dishes)")
showPlot(diet198, 4, "Carbohydrates (198 dishes)")
showPlot(diet198, 5, "Fats (198 dishes)")


