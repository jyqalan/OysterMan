if(!isGeneric("get_value")) setGeneric("get_value", function(object, ...)standardGeneric("get_value"))

setGeneric("Ogive_logistic", function(object, ...)standardGeneric("Ogive_logistic"))

setClass("Ogive_logistic",representation("Ogive",a50="numeric",a_to95="numeric"))
setMethod("initialize", signature(.Object="Ogive_logistic"),
          function(.Object,low,high,a50, a_to95){


   
  .Object@low=low
  .Object@high=high
  .Object@by_class = FALSE
  .Object@a50=a50
  .Object@a_to95=a_to95
   return (.Object)
          }
)  
setMethod("Ogive_logistic", signature(object="missing"),
	function(object,low, high, a50, a_to95) {
		return(new("Ogive_logistic",low=low,high=high,a50=a50,a_to95=a_to95))
	}
)
setMethod("get_value", signature(object="Ogive_logistic"),
	function(object,x){
          temp=(object@a50-x)/object@a_to95
          result=ifelse(temp > 5.0, 0.0, ifelse(temp < -5.0, 1.0, 1.0/(1.0+19.0^temp)))
          return (result)
      }
)

