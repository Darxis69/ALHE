library(hash)
library(digest)

#TODO WRite in documentation that tabuSize is unlimited
tabuSearch <- function(startingPoint, stopConditionFunc, neighborHoodFunc, evaluateFunc)
{
  tabu <- hash()
  evaluateValues <- hash()
  bestPoint <- startingPoint
  bestPointEvaluate <- evaluateFunc(bestPoint)
  observationsCount <- 0
  
  while (!stopConditionFunc(bestPoint))
  {
    neighborHood <- neighborHoodFunc(bestPoint)
    bestCandidate <- NULL
    bestCandidateEvaluate <- 0
    bestCandidateChecksum <- NULL
    for (candidate in neighborHood)
    {
      #TODO Profile this find waskie gardlo
      candidateChecksum = digest(candidate)
      if (!has.key(candidateChecksum, tabu))
      {
        if (has.key(candidateChecksum, evaluateValues))
        {
          candidateEvaluate <- evaluateValues[[candidateChecksum]]
        }
        else 
        {
          candidateEvaluate <- evaluateFunc(candidate)
          .set(evaluateValues, keys=candidateChecksum, values=candidateEvaluate)
        }
        
        if (is.null(bestCandidate) || candidateEvaluate > bestCandidateEvaluate)
        {
          bestCandidateEvaluate <- candidateEvaluate
          bestCandidateChecksum <- candidateChecksum
          bestCandidate <- candidate
        }
      }
    }
    
    if (is.null(bestCandidate))
    {
      break
    }
    
    #TODO Is the following 'if' required?
    #'Różnica między metodą tabu a zwykłym algorytmem wspinaczki
    # polega na tym, że zezwalamy na odwiedzanie sąsiadów o jakości
    # gorszej od aktualnie rozpatrywanej'
    
    #Retest
    #if (bestCandidateEvaluate > bestPointEvaluate)
    bestPoint <- bestCandidate
    bestPointEvaluate <- bestCandidateEvaluate
    
    observationsCount <- observationsCount + 1
    
    .set(tabu, keys=digest(bestCandidate), values=0)
  }
  
  rm(tabu)
  return(list(bestPoint, observationsCount))
}
