if(!isGeneric("SR")) setGeneric("SR", function(object, ...)standardGeneric("SR"))

setGeneric("Beverton_Holt", function(object, ...)standardGeneric("Beverton_Holt"))

setClass("Beverton_Holt",representation("Stock_recruit",h="numeric",B0="numeric"))
setMethod("initialize", signature(.Object="Beverton_Holt"),
          function(.Object, h, B0){
              .Object@h=h
              .Object@B0=B0
              return (.Object)
          }
)   
setMethod("Beverton_Holt", signature(object="missing"),
	function(object,h,B0) {
		return(new("Beverton_Holt",h=h,B0=B0))
	}
)
setMethod("SR", signature(object="Beverton_Holt"),
	function(object,SSB) {
             return ((SSB/object@B0) / (1-((5*object@h-1)/(4*object@h))*(1-SSB/object@B0)))
	}
)        

