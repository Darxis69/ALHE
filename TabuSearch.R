library(hash)
library(digest)

tabuSearch <- function(tabuSize, startingPoint, stopConditionFunc, neighborHoodFunc, evaluateFunc)
{
  tabu <- hash()
  evaluateValues <- hash()
  bestPoint <- startingPoint
  
  while (!stopConditionFunc(bestPoint))
  {
    neighborHood <- neighborHoodFunc(bestPoint)
    bestCandidate <- NULL
    bestCandidateEvaluate <- 0
    bestCandidateChecksum <- NULL
    print("BEGIN")
    for (candidate in neighborHood)
    {
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
    print("END")
    
    if (is.null(bestCandidate))
    {
      break
    }
    
    if (bestCandidateEvaluate > evaluateFunc(bestPoint))
    {
      bestPoint <- bestCandidate
    }
    
    .set(tabu, keys=digest(bestCandidate), values=0)
    
    ##TODO Remove tabu elements when tabuSize exceeded
    ##For this, create a FIFO queue with hashes in order of inserting
  }
  
  rm(tabu)
  return(bestPoint)
}
