bootstrp=function(a,b,number_of_permutations) {
  combo=c(a,b)
  diff.observed = mean(a) - mean(b)
  diff.random=numeric(number_of_permutations)
  for (i in 1 : number_of_permutations) {
    
    # Sample from the combined dataset without replacement
    shuffled = sample (combo, length(combo))
    
    a.random = shuffled[1 : length(a)] 
    b.random = shuffled[(length(a) + 1) : length(combo)]
    
    # Null (permuated) difference
    diff.random[i] = mean(a.random) - mean(b.random)
  }
  pvalue = sum(abs(diff.random) >= abs(diff.observed)) /
    number_of_permutations
  rt=sum((diff.random) >= (diff.observed)) / number_of_permutations
  lt=sum((diff.random) < (diff.observed)) / number_of_permutations
  result<-list(origDiff=diff.observed,
               pval=pvalue,
               diffs=as.numeric(diff.random),
               rtail=rt,ltail=lt)
  cat("Difference was ",diff.observed,"\n",
      "probability of observing = ",pvalue,"\n")
  return(result)
}

