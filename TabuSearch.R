listContainsElement <- function(l, element)
{
  listSize <- length(l)
  if (listSize > 0)
  {
    for (i in 1:listSize)
    {
      if (identical(l[[i]], element))
      {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}

tabuSearch <- function(tabuSize, startingPoint, stopConditionFunc, neighborHoodFunc, evaluateFunc)
{
  tabu <- list()
  bestPoint <- startingPoint
  
  while (!stopConditionFunc(bestPoint))
  {
    neighborHood <- neighborHoodFunc(bestPoint)
    bestCandidate <- NULL
    bestCandidateEvaluate <- 0
    for (candidate in neighborHood)
    {
      if (!(listContainsElement(tabu, candidate)))
      {
        candidateEvaluate <- evaluateFunc(candidate)
        if (is.null(bestCandidate) || candidateEvaluate > bestCandidateEvaluate)
        {
          bestCandidateEvaluate <- candidateEvaluate
          bestCandidate <- candidate
        }
      }
    }
    
    if (is.null(bestCandidate))
    {
      break
    }
    
    if (bestCandidateEvaluate > evaluateFunc(bestPoint))
    {
      bestPoint <- bestCandidate
    }
    
    tabu <- append(tabu, list(bestCandidate))
    if (length(tabu) > tabuSize)
    {
      tabu <- tabu[-1]
    }
  }
  
  return(bestPoint)
}