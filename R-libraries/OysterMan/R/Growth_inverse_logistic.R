setGeneric("Growth_inverse_logistic", function(object, ...)standardGeneric("Growth_inverse_logistic"))



setClass("Growth_inverse_logistic",representation("Growth",gmax="numeric",a50="numeric",a_to95="numeric",alpha="numeric",beta="numeric",minsigma="numeric",delta="numeric"))

setMethod("initialize", signature(.Object="Growth_inverse_logistic"),
          function(.Object,n_classes,class_mins,plus_group,plus_group_size,gmax,a50,a_to95,alpha,beta,minsigma=0,delta=1){
              if(gmax < 0 ) stop("gmax < 0")
              if(a50 < 0 ) stop("a50 < 0")
              if(a_to95 < 0 ) stop("a_to95 < 0")
              if(alpha < 0 ) stop("alpha < 0")
              if(beta < 0 ) stop("beta < 0")
              if(minsigma < 0) stop("minsigma <0")
              if(delta < 0) stop("delta <0")
              .Object@gmax = gmax
              .Object@a50 = a50
              .Object@a_to95 = a_to95
              .Object@alpha = alpha
              .Object@beta = beta
              .Object@minsigma = minsigma              
              .Object@delta = delta

              .Object@transition_matrix=make_transition_matrix(object=.Object,n_classes=n_classes,class_mins=class_mins,plus_group=plus_group,plus_group_size=plus_group_size)
              return (.Object)
           }
)

setMethod("Growth_inverse_logistic", signature(object="missing"),
	function(object,n_classes,class_mins,plus_group,plus_group_size,gmax,a50,a_to95,alpha,beta,minsigma=0,delta=1){
            return(new("Growth_inverse_logistic",n_classes=n_classes,
                               class_mins=class_mins,
                               plus_group=plus_group,
                               plus_group_size=plus_group_size,
                               gmax=gmax,a50=a50,a_to95=a_to95,alpha=alpha,beta=beta,minsigma=minsigma,delta=delta))
	}
)


setMethod("get_mu", signature(object="Growth_inverse_logistic"),
	function(object,l) {
            mu=object@delta*object@gmax/(1+exp(log(19)*((l-object@a50)/(object@a_to95))))
             return (mu)
 	}
)

setMethod("get_stdev", signature(object="Growth_inverse_logistic"),
	function(object,l) {
            mu = get_mu(object,l)
            stdev <- max(object@minsigma,object@alpha*mu^(object@beta))
     return (stdev)
 }
)

