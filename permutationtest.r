#
# Monte Carlo method for blockwise permutation test for 
# comparison of means
#
# References:
# 
# W.J.Welch (1990). Construction of Permutation Tests. 
# Journal of the American Statistical Association 85(411), pp.693-698.
# 
# 

#
# idxindiv = index vector of individuals (possibly, the same individual 
#       is present in several rows)
# x, y = vectors of individual observations under conditions 1 and 2 
# B = number of permutations
# alternative: a character string specifying the alternative hypothesis, 
# must be one of "two.sided" (default), "greater" or "less". 
# You can specify just the initial letter.
BlockPermTest = function(idxindiv, x, y, B=500, paired=FALSE, alternative='two.sided') {
  
  statistic = ifelse(paired, mean(x-y), mean(x)-mean(y))

  individuals = sort(unique(idxindiv))
  szsmp = length(individuals)
  StatDist = rep(0, B+1)
  for (idxs in 1:B) {
    xs = x
    ys = y
    coins = rbinom(szsmp,1,0.5)
    idxperm = which(idxindiv %in% which(coins == 1))
    if (length(idxperm)>0) {
      xs[idxperm] = y[idxperm]
      ys[idxperm] = x[idxperm]
    }
    StatDist[idxs] =  ifelse(paired, mean(xs-ys), mean(xs)-mean(ys))
  }
  StatDist[B+1] = statistic

  if (alternative=='two.sided') {
    p.value = length(which(abs(StatDist)>=abs(statistic)))/(B+1)
  }  
  
  if (alternative=='greater') {
    p.value = ifelse(statistic>=0, length(which(StatDist>=statistic))/(B+1), 1)
  }  

  if (alternative=='less') {
    p.value = ifelse(statistic<=0, length(which(StatDist<=statistic))/(B+1), 1)
  }  
  
  return(list(statistic=statistic, p.value=p.value, StatDist=StatDist))
}






