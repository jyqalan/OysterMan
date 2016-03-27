if(!isGeneric("get_transition_matrix")) setGeneric("get_transition_matrix", function(object, ...)standardGeneric("get_transition_matrix"))
if(!isGeneric("make_transition_matrix")) setGeneric("make_transition_matrix", function(object, ...)standardGeneric("make_transition_matrix"))
if(!isGeneric("get_mu")) setGeneric("get_mu", function(object, ...)standardGeneric("get_mu"))
if(!isGeneric("get_stdev")) setGeneric("get_stdev", function(object, ...)standardGeneric("get_stdev"))

setClass("Growth",representation(transition_matrix="matrix"))


setMethod("get_transition_matrix", signature(object="Growth"),
	function(object) {
            return (object@transition_matrix)
 	}
)

setMethod("get_mu", signature(object="Growth"),
	function(object,l) {
 	}
)

setMethod("get_stdev", signature(object="Growth"),
	function(object,l) {
 	}
)

setMethod("make_transition_matrix", signature(object="Growth"),
	function(object,n_classes,class_mins,plus_group,plus_group_size) {
            
  l_c = vector("numeric",length=n_classes);
  i = 1
  while (i < n_classes){
    l_c[i] = (class_mins[i]+class_mins[i+1])*0.5
    i = i+1
  }
  if (!plus_group){
    l_c[n_classes] = (class_mins[n_classes]+class_mins[n_classes])*0.5
  }
  transition = matrix(0,nrow=n_classes,ncol=n_classes)

  i = 1
  while(i<=n_classes){
      j = 1
      while (j<i){
          transition[i,j] = 0;
          j=j+1
      }
      if (plus_group && i==n_classes){
          transition[i,i] = 1;
      } else {
          mu = get_mu(object,l_c[i])
          stdev = get_stdev(object,l_c[i])
          transition[i,i] = pnorm(class_mins[i+1]-l_c[i],mean=mu,sd=stdev)
          sum_so_far = transition[i,i]
          j=i+1
          while(j<n_classes){
              transition[i,j] <- pnorm(class_mins[j+1]-l_c[i],mean=mu,sd=stdev) - sum_so_far
              sum_so_far = sum_so_far+transition[i,j]
              j=j+1
          }
          if (plus_group){
              transition[i,n_classes] = 1 - sum_so_far
          } else {
              transition[i,n_classes] = pnorm(class_mins[n_classes+1]-l_c[i],mean=mu,sd=stdev) - sum_so_far
          }
      }
      i <- i+1
  }
  return (transition)
}
) 
   
