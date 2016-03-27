if(!isGeneric("get_value")) setGeneric("get_value", function(object, ...)standardGeneric("get_value"))

setGeneric("Ogive_constant", function(object, ...)standardGeneric("Ogive_constant"))

setClass("Ogive_constant",representation("Ogive",constant="numeric"))
setMethod("initialize", signature(.Object="Ogive_constant"),
          function(.Object, low, high, constant){
              .Object@low=low
              .Object@high=high
              .Object@by_class = FALSE
              .Object@constant=constant
              return (.Object)
          }
)  
setMethod("Ogive_constant", signature(object="missing"),
	function(object,low, high, constant) {
		return(new("Ogive_constant",low=low,high=high,constant=constant))
	}
)
setMethod("get_value", signature(object="Ogive_constant"),
	function(object,x){
          result=rep(object@constant,length(x))
          return (result)
      }
)

