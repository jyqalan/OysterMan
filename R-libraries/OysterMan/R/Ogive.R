if(!isGeneric("get_value")) setGeneric("get_value", function(object, ...)standardGeneric("get_value"))

setClass("Ogive",representation(low="numeric",high="numeric",by_class="logical"))

setClassUnion("Ogive_or_NULL",c("Ogive","NULL"))

setMethod("get_value", signature(object="Ogive"),
	function(object,x){
      }
)   
