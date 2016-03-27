pow <- function(x,y) x^y

posfun <- function(x,eps=0.05){
    ifelse(x>=eps,x,eps/(2-x/eps))
}
   



distribution <- function(class_mins, plus_group = FALSE,dist = "normal", mean=0, stdev=1){
  # Probability distribution of a random variable over classes.
  # distribution(class_mins,plus_group,dist,mean,stdev)[i]
  # is the probability that a random variable with distribution 'dist', 'mean' and 'stdev'
  # falls between class_mins[i] and class_mins[i+1]
  # (unless i = class_mins[length(class_mins)] and plus_group!=0, in which case
  # distribution(...)[i] is the probability that the random variable exceeds class_mins[i]).
  
  # We use an approximation: P(X is more than 5 std.devs away from its mean) = 0.
  # Almost true for the normal distribution, but may be problematic if you use something more skewed.
  first_class = 1
  last_class = length(class_mins) - ifelse(plus_group, 0, 1)
  result = vector("numeric",length=last_class)
  if (dist=="lognormal"){
    sigma = sqrt(log(1+pow(stdev/mean,2)))
    mu = log(mean) - sigma*sigma/2
  }
  if (class_mins[first_class] < mean-5*stdev){
    so_far = 0
  } else {
    if (dist=="normal"){
      so_far = pnorm(class_mins[first_class],mean,stdev)
    } else if (dist=="lognormal"){
      so_far = plnorm(class_mins[first_class],mu,sigma)
    } else stop(paste("Unknown dist =",dist,"in distribution()"))
  }
  i=first_class
  while (i < last_class){
    if (class_mins[i+1] > mean+5*stdev){
      result[i] = 1-so_far
      so_far = 1
    } else if (class_mins[i+1] < mean-5*stdev){
      result[i] = 0
      so_far = 0
    } else {
      if (dist=="normal"){
        result[i] = pnorm(class_mins[i+1],mean,stdev) - so_far
      } else if (dist=="lognormal"){
        result[i] = plognorm(class_mins[i+1],mu,sigma) - so_far
      }
      so_far = so_far+result[i]
    }
    if (result[i]<0){ stop("bad result in distribution")}
    i=i+1
  }
  i = last_class
  if (plus_group){
    result[i] = 1-so_far
  } else {
    if (class_mins[i+1] > mean+5*stdev){
      result[i] = 1-so_far
    } else {
      if (dist=="normal"){
        result[i] = pnorm(class_mins[i+1],mean,stdev) - so_far
      } else if (dist=="lognormal"){
        result[i] = plognorm(class_mins[i+1],mu,sigma) - so_far
      }
    }
    if (result[i]<0){stop("bad result in distribution")}
   }
  return (result)
}











