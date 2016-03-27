# Partition Class
if(!isGeneric("print")) setGeneric("print", useAsDefault = print)
if(!isGeneric("show")) setGeneric("show", useAsDefault = show)
if(!isGeneric("initialize")) setGeneric("initialize", useAsDefault = initialize)
if(!isGeneric("get_row")) setGeneric("get_row", function(object, ...)standardGeneric("get_row"))
if(!isGeneric("average_partition")) setGeneric("average_partition",function(object,...) standardGeneric("average_partition"))

setClass("Partition",representation("vector",col_min="numeric",col_max="numeric",n_cols="numeric"))
setMethod("initialize", signature(.Object="Partition"),
          function(.Object,col_min=1,col_max=1){
              if(col_min != 1) stop("col_min != 1")
              if(col_min > col_max) stop("col_min > col_max")                            
              .Object@col_min=col_min
              .Object@col_max=col_max
              .Object@n_cols = col_max - col_min + 1
              .Object@.Data = vector("numeric",length=.Object@n_cols)
              return (.Object)
          }
)
setMethod("show", signature(object="Partition"),
	function(object){
                cat(object@.Data,"\n")
	}
)  
setMethod("print", signature(x="Partition"),
	function(x){
            show(x)
	}
)  
setMethod("get_row",signature(object="Partition"),
          function(object){
                return (object@.Data)
            }
)          

setMethod("average_partition",signature(object="Partition"),
          function(object,before,proportion,method="weighted_sum"){
              if (proportion==1){
                  during = object@.Data
              } else if (proportion==0){
                  during = before
              } else {
                  if (method=="weighted_sum"){
                      during=object@.Data*proportion + before*(1-proportion)
                  } else if (method=="weighted_product"){
                      during=pow(object@.Data,proportion) * pow(before,1-proportion)
                  } else stop(paste("Unknown method ",method," in Partition::average_partition",sep=""))
              }
              return(during)
          }
)
