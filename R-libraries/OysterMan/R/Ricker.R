if(!isGeneric("SR")) setGeneric("SR", function(object, ...)standardGeneric("SR"))

setGeneric("Ricker", function(object, ...)standardGeneric("Ricker"))


setClass("Ricker",representation("Stock_recruit",h="numeric",B0="numeric"))
setMethod("initialize", signature(.Object="Ricker"),
          function(.Object, h, B0){
              .Object@h=h
              .Object@B0=B0
              return (.Object)
          }
)   
setMethod("Ricker", signature(object="missing"),
	function(object,h,B0) {
		return(new("Ricker",h=h,B0=B0))
	}
)
setMethod("SR", signature(object="Ricker"),
	function(object,SSB) {
               return (SSB/object@B0) * pow((1/(5*object@h)),(1.25*(SSB/object@B0-1)))
	}
)
