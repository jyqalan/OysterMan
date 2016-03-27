if(!isGeneric("get_selectivities")) setGeneric("get_selectivities", function(object, ...)standardGeneric("get_selectivities"))


setGeneric("Selectivity", function(object, ...)standardGeneric("Selectivity"))

setClass("Selectivity",representation(ogive="Ogive",selectivities="vector"))
setMethod("initialize", signature(.Object="Selectivity"),
          function(.Object,ogive,n_classes, class_mins, plus_group, plus_group_size){
  low = 1
  high = n_classes
  class_sizes = vector("numeric",length=n_classes)
  for (i in low:high){
      class_sizes[i] = (class_mins[i] + class_mins[i+1]) * 0.5;
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
      .Object@selectivities=get_value(ogive,ogive@low:ogive@high)
  } else {
      .Object@selectivities=get_value(ogive,class_sizes)
  }
   return (.Object)
  }
)
setMethod("Selectivity", signature(object="missing"),
	function(object,ogive,n_classes, class_mins, plus_group, plus_group_size) {
		return(new("Selectivity",ogive=ogive,n_class=n_classes,class_mins=class_mins, plus_group=plus_group, plus_group_size=plus_group_size))
	}
)
setMethod("get_selectivities", signature(object="Selectivity"),
	function(object){
          return (object@selectivities)
      }
)
