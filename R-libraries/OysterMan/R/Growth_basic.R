setGeneric("Growth_basic", function(object, ...)standardGeneric("Growth_basic"))



setClass("Growth_basic",representation("Growth",l1="numeric",l2="numeric",g1="numeric",g2="numeric",alpha="numeric",beta="numeric",minsigma="numeric",delta="numeric"))

setMethod("initialize", signature(.Object="Growth_basic"),
          function(.Object,n_classes,class_mins,plus_group,plus_group_size,l1,l2,g1,g2,alpha,beta,minsigma=0,delta=1){
              if(l1 < 0 ) stop("l1 < 0")
              if(l2 < 0 ) stop("l2 < 0")
              if(g1 < 0 ) stop("g1 < 0")
              if(g2 < 0 ) stop("g2 < 0")
              if(alpha < 0 ) stop("alpha < 0")
              if(beta < 0 ) stop("beta < 0")
              if(minsigma < 0) stop("minsigma <0")
              if(delta < 0) stop("delta <0")
              .Object@l1 = l1
              .Object@l2 = l2
              .Object@g1 = g1
              .Object@g2 = g2
              .Object@alpha = alpha
              .Object@beta = beta
              .Object@minsigma = minsigma              
              .Object@delta = delta

              .Object@transition_matrix=make_transition_matrix(object=.Object,n_classes=n_classes,class_mins=class_mins,plus_group=plus_group,plus_group_size=plus_group_size)
              return (.Object)
           }
)

setMethod("Growth_basic", signature(object="missing"),
	function(object,n_classes,class_mins,plus_group,plus_group_size,l1,l2,g1,g2,alpha,beta,minsigma=0,delta=1){
            return(new("Growth_basic",n_classes=n_classes,
                               class_mins=class_mins,
                               plus_group=plus_group,
                               plus_group_size=plus_group_size,
                               l1=l1,l2=l2,g1=g1,g2=g2,alpha=alpha,beta=beta,minsigma=minsigma,delta=delta))
	}
)


setMethod("get_mu", signature(object="Growth_basic"),
	function(object,l) {
            temp = posfun(1+(object@g1-object@g2)/(object@l1-object@l2));       
            mu <- posfun(((object@l2*object@g1-object@l1*object@g2)/(object@g1-object@g2)-l)*(1-temp^object@delta))
             return (mu)
 	}
)

setMethod("get_stdev", signature(object="Growth_basic"),
	function(object,l) {
            mu = get_mu(object,l)
            stdev <- max(object@minsigma,object@alpha*mu^(object@beta))
     return (stdev)
 }
)

