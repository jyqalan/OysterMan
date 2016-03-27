if(!isGeneric("SR")) setGeneric("SR", function(object, ...)standardGeneric("SR"))

setClass("Stock_recruit")
setClassUnion("Stock_recruit_or_NULL",c("Stock_recruit","NULL"))
setMethod("SR", signature(object="Stock_recruit"),
	function(object,SSB) {
	}
)

   
