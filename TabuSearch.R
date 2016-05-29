library(hash)
library(digest)

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
  tabu <- hash()
  bestPoint <- startingPoint
  
  while (!stopConditionFunc(bestPoint))
  {
    neighborHood <- neighborHoodFunc(bestPoint)
    bestCandidate <- NULL
    bestCandidateEvaluate <- 0
    for (candidate in neighborHood)
    {
      if (!has.key(digest(candidate), tabu))
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
    
    .set(tabu, keys=list(digest(bestCandidate)), values=list(bestCandidate))
    
    ##TODO Remove tabu elements when tabuSize exceeded
    ##For this, create a FIFO queue with hashes in order of inserting
  }
  
  rm(tabu)
  return(bestPoint)
}
