if(!isGeneric("get_M")) setGeneric("get_M", function(object, ...)standardGeneric("get_M"))
if(!isGeneric("get_M_by_element")) setGeneric("get_M_by_element", function(object, ...)standardGeneric("get_M_by_element"))
if(!isGeneric("get_exp_minus_M_by_element")) setGeneric("get_exp_minus_M_by_element", function(object, ...)standardGeneric("get_exp_minus_M_by_element"))

setGeneric("Natural_mortality", function(object, ...)standardGeneric("Natural_mortality"))

setClass("Natural_mortality",representation(M="numeric", ogive="Ogive_or_NULL",M_by_element="vector",exp_minus_M_by_element="vector"))
setMethod("initialize", signature(.Object="Natural_mortality"),
          function(.Object,M,ogive,n_classes, class_mins, plus_group, plus_group_size){
              .Object@M=M
              .Object@ogive=ogive
              if(is.null(ogive)){
                  .Object@M_by_element=rep(M,n_classes)
              } else {
                  low = 1
                  high = n_classes
                  class_sizes = vector("numeric",length=n_classes)
                  for (i in low:high){
                      class_sizes[i] = class_mins[i] + class_mins[i+1] * 0.5;
                  }
                  if (plus_group){
                      if (length(class_mins) != (high-low+1)) stop("class_mins is the wrong size")
                      class_sizes[high] = plus_group_size
                  } else {
                      if (length(class_mins) != (high-low+2)) fatal("class_mins is the wrong size");
                      class_sizes[high] = (class_mins[high] + class_mins[high+1]) * 0.5;
                  }
                  if(ogive@low != 1) stop("ogive@low != 1")
                  if(ogive@high != n_classes) stop("ogive@high != n_classe")
                  .Object@ogive=ogive
                  if(ogive@by_class){
                      .Object@M_by_element=M*get_value(ogive,ogive@low:ogive@high)
                  } else {
                      .Object@M_by_element=M*get_value(ogive,class_sizes)
                  }
                  
              }
              .Object@exp_minus_M_by_element=exp(-.Object@M_by_element)
              return (.Object)
          }
)
setMethod("Natural_mortality", signature(object="missing"),
	function(object,M,ogive,n_classes, class_mins, plus_group, plus_group_size) {
		return(new("Natural_mortality",M=M,ogive=ogive,n_class=n_classes,class_mins=class_mins, plus_group=plus_group, plus_group_size=plus_group_size))
	}
)
setMethod("get_M", signature(object="Natural_mortality"),
	function(object){
          return (object@M)
      }
)
setMethod("get_M_by_element", signature(object="Natural_mortality"),
	function(object){
          return (object@M_by_element)
      }
)
setMethod("get_exp_minus_M_by_element", signature(object="Natural_mortality"),
	function(object){
          return (object@exp_minus_M_by_element)
      }
)
