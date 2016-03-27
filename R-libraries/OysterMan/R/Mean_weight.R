if(!isGeneric("get_mean_weight")) setGeneric("get_mean_weight", function(object, ...)standardGeneric("get_mean_weight"))

setClass("Mean_weight",representation(mean_weight="vector"))
setMethod("get_mean_weight", signature(object="Mean_weight"),
	function(object) {
            return (object@mean_weight)
 	}
)   
