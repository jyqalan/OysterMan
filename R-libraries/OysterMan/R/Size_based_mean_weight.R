if(!isGeneric("mean_weight_at_size")) setGeneric("mean_weight_at_size", function(object, ...)standardGeneric("mean_weight_at_size"))   

setGeneric("Size_based_mean_weight", function(object, ...)standardGeneric("Size_based_mean_weight"))

setClass("Size_based_mean_weight",representation("Mean_weight",a="numeric",b="numeric"))
setMethod("initialize", signature(.Object="Size_based_mean_weight"),
          function(.Object, a,b,n_classes,class_mins,plus_group,plus_group_size){

         class_sizes=vector("numeric",length=n_classes)
         j=1
         while(j<n_classes){
             class_sizes[j] = (class_mins[j]+class_mins[j+1]) * 0.5
             j=j+1
         }
         if (plus_group){
             class_sizes[n_classes] = plus_group_size
         } else {
             class_sizes[n_classes] = (class_mins[n_classes]+class_mins[n_classes+1]) * 0.5
         }
              
        .Object@a=a
        .Object@b=b
        .Object@mean_weight=mean_weight_at_size(.Object,class_sizes)
         return (.Object)
          }
)   
setMethod("Size_based_mean_weight", signature(object="missing"),
	function(object,a,b,n_classes,class_mins,plus_group,plus_group_size) {
		return(new("Size_based_mean_weight",a=a,b=b,n_classes=n_classes,class_mins=class_mins,plus_group=plus_group,plus_group_size=plus_group_size))
	}
)
setMethod("mean_weight_at_size", signature(object="Size_based_mean_weight"),
	function(object,l) {
            result =  object@a*pow(l,object@b)
            return(result)
 	}
)    
