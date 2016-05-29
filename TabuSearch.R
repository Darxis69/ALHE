tabuSearch <- function(tabuSize, startingPoint, stopConditionFunc, neighborHoodFunc, evaluateFunc)
{
  tabu <- list()
  bestPoint <- startingPoint
  
  while (!stopConditionFunc(bestPoint))
  {
    neighborHood <- neighborHoodFunc(bestPoint)
    bestCandidate <- NULL
    bestCandidateEvaluate <- -1000000
    for (candidate in neighborHood)
    {
      if (!(candidate %in% tabu))
      {
        candidateEvaluate <- evaluateFunc(candidate)
        if (candidateEvaluate > bestCandidateEvaluate)
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