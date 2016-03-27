if(!isGeneric("print")) setGeneric("print", useAsDefault = print)
if(!isGeneric("show")) setGeneric("show", useAsDefault = show)
if(!isGeneric("initialize")) setGeneric("initialize", useAsDefault = initialize)
if(!isGeneric("abundance")) setGeneric("abundance", function(object, ...)standardGeneric("abundance"))
if(!isGeneric("number")) setGeneric("number", function(object, ...)standardGeneric("number"))
if(!isGeneric("mature_abundance")) setGeneric("mature_abundance", function(object, ...)standardGeneric("mature_abundance"))
if(!isGeneric("mature_number")) setGeneric("mature_number", function(object, ...)standardGeneric("mature_number"))
if(!isGeneric("recruitment_process")) setGeneric("recruitment_process", function(object, ...)standardGeneric("recruitment_process"))
if(!isGeneric("M_process")) setGeneric("M_process", function(object, ...)standardGeneric("M_process"))
if(!isGeneric("growth_process")) setGeneric("growth_process", function(object, ...)standardGeneric("growth_process"))
if(!isGeneric("spawning_process")) setGeneric("spawning_process", function(object, ...)standardGeneric("spawning_process"))
if(!isGeneric("catch_process")) setGeneric("catch_process", function(object, ...)standardGeneric("catch_process"))
if(!isGeneric("F_process")) setGeneric("F_process", function(object, ...)standardGeneric("F_process"))
if(!isGeneric("anuual_cycle")) setGeneric("annual_cycle", function(object, ...)standardGeneric("annual_cycle"))



if(!isGeneric("set_initial_state")) setGeneric("set_initial_state", function(object, ...)standardGeneric("set_initial_state"))
if(!isGeneric("set_current_state")) setGeneric("set_current_state", function(object, ...)standardGeneric("set_current_state"))
if(!isGeneric("YPR_SPR")) setGeneric("YPR_SPR", function(object, ...)standardGeneric("YPR_SPR"))
if(!isGeneric("deterministic_MSY")) setGeneric("deterministic_MSY", function(object, ...)standardGeneric("deterministic_MSY"))

setGeneric("State", function(object, ...)standardGeneric("State"))
setClassUnion("function_or_NULL",c("function","NULL"))


# State Class
setClass("State",representation("Partition",
                                n_classes="numeric",
                                class_mins="vector",
                                plus_group="logical",
                                plus_group_size="numeric",
                                initial_size_mean="numeric",
                                initial_size_cv="numeric",
                                initial_size_dist="vector",
                                growth="Growth",
                                maturity="Maturity",
                                natural_mortality="Natural_mortality",
                                fishing_selectivity="Selectivity",
                                mean_weight="Mean_weight",
                                stock_recruit="Stock_recruit_or_NULL",
                                R0="numeric",
                                B0="numeric",
                                SSB="numeric",
                                U_max="numeric",
                                U_obs="numeric",
                                U="numeric",
                                F_max="numeric",
                                F_obs="numeric",
                                F="numeric",
                                catch="numeric",
                                actual_catch="numeric",
                                fishing_pressure_limit_exceeded="logical",
                                annual_cycle="function_or_NULL"))

setMethod("initialize", signature(.Object="State"),
          function(.Object,n_classes,class_mins,plus_group,plus_group_size,initial_size_mean,initial_size_cv,growth,maturity,natural_mortality,fishing_selectivity,mean_weight,stock_recruit,U_max,F_max,annual_cycle=NULL){
              
              if (plus_group) {
                  if (length(class_mins) != (n_classes)) stop("class_mins is the wrong size")
              } else {
                  if (length(class_mins) != (n_classes+1)) stop("class_mins is the wrong size")
              }
              for(i in 1:length(class_mins)){
                  if(class_mins[i] < 0) stop("class_mins < 0")
              }      
              for(i in 1:(length(class_mins)-1)){
                  if(class_mins[i] > class_mins[i+1]) stop("class_mins should not be decreasing")
              }
              if(plus_group){
                  if(plus_group_size < 0) stop("plus_group_size < 0")
              }

              if(U_max < 0 || U_max >= 1) stop("U_max must be between 0 and 1")
              if(initial_size_mean <= 0) stop("initial_size_mean <= 0")
              if(initial_size_cv <= 0) stop("initial_size_cv <= 0")             

              .Object@n_classes = n_classes         
              .Object@class_mins = class_mins
              .Object@plus_group = plus_group
               if(plus_group){
                  .Object@plus_group_size = plus_group_size
               } else {
                   .Object@plus_group_size = -1
               }
              .Object@n_cols = n_classes
              .Object@col_min = 1
              .Object@col_max =  n_classes
              .Object@.Data = vector("numeric",length=.Object@n_cols)

              .Object@initial_size_mean=initial_size_mean
              .Object@initial_size_cv=initial_size_cv
              .Object@initial_size_dist = distribution(class_mins,plus_group,"normal",initial_size_mean, initial_size_mean * initial_size_cv)
              .Object@growth = growth
              .Object@maturity = maturity
              .Object@natural_mortality = natural_mortality
              .Object@fishing_selectivity = fishing_selectivity
              .Object@mean_weight = mean_weight
              .Object@stock_recruit = stock_recruit
              .Object@R0 = 0
              .Object@B0 = 0
              .Object@SSB = 0
              .Object@U_max = U_max
              .Object@F_max = F_max
              .Object@U=0
              .Object@U_obs=0
              .Object@F=0
              .Object@F_obs=0
              .Object@catch=0
              .Object@actual_catch=0
              .Object@fishing_pressure_limit_exceeded=FALSE
              
              if(is.null(annual_cycle)) .Object@annual_cycle = NULL
              else .Object@annual_cycle = match.fun(annual_cycle)
              return (.Object)
           }
)

setMethod("State", signature(object="missing"),
	function(object,n_classes,class_mins,plus_group,plus_group_size, initial_size_mean,initial_size_cv,growth,
                 maturity,natural_mortality,fishing_selectivity,size_at_age,mean_weight,stock_recruit,U_max,F_max,annual_cycle=NULL) {
            return(new("State",n_classes=n_classes,
                               class_mins=class_mins,
                               plus_group=plus_group,
                               plus_group_size=plus_group_size,
                               initial_size_mean=initial_size_mean,
                               initial_size_cv=initial_size_cv,
                               growth=growth,
                               maturity=maturity,
                               natural_mortality=natural_mortality,
                               fishing_selectivity=fishing_selectivity,
                               mean_weight=mean_weight,
                               stock_recruit=stock_recruit,
                               U_max=U_max,
                               F_max=F_max,
                               annual_cycle=annual_cycle))
	}
)

setMethod("show", signature(object="State"),
	function(object){
                cat(object@.Data,"\n")            
		cat("n_classes: ", object@n_classes, "\n")
		cat("class_mins: ", object@class_mins, "\n")
 		cat("B0: ", object@B0, "\n")
	 	cat("R0: ", object@R0, "\n")
		cat("SSB: ", object@SSB, "\n")               
	}
)
setMethod("print", signature(x="State"),
          function(x){
             show(x)
         }
)

setMethod("abundance", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
            result <- sum(result * get_mean_weight(object@mean_weight))
            return(result)
	}
)

setMethod("mature_abundance", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)*get_mature_proportions(object@maturity)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
            result <- sum(result * get_mean_weight(object@mean_weight))
            return(result)
	}
)

setMethod("number", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
           return(result)
	}
)

setMethod("mature_number", signature(object="State"),
	function(object,selectivity=NULL){
            result <- get_row(object)*get_mature_proportions(object@maturity)
            if(!is.null(selectivity)){
                result <- result * get_selectivities(selectivity)
            }
            return(result)
	}
)

setMethod("annual_cycle",signature(object="State"),
        function(object,...){
            if(is.null(annual_cycle)) stop("annual_cycle not defined!")
            else return(object@annual_cycle(object,...))
        }         
)

setMethod("set_initial_state",signature(object="State"),
        function(object,B0,n_equilibrium){

     if(n_equilibrium < 1) stop("n_equilibrium < 1!")
     if(is.null(object@annual_cycle)) stop("annual_cycle not defined!")
     object@R0 <- 1
     object@.Data <- vector("numeric",length=object@n_cols)
     for(i in 1:n_equilibrium) {
         object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=NULL)
     }
     plus_group_total=object@.Data[object@col_max]
     object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=NULL)
     plus_group_total_later=object@.Data[object@col_max]

     if (plus_group_total_later > 1.05 * plus_group_total){
        cat("Warning: your plus group grew by over 5% in your last equilibrium year. You should probably increase n_equilibrium and try again.\n\n")
        cat("         Biomass =",plus_group_total," in year ",n_equilibrium-1,"\n")
        cat("         Biomass =",plus_group_total_later, " in year ", n_equilibrium,"\n")
      }
      SSB <- object@SSB
      object@.Data <-  object@.Data*B0/SSB
      object@B0 <- B0
      object@R0 <- B0/SSB            
      object@SSB <- B0
     return(object)
 }
)



setMethod("set_current_state",signature(object="State"),
        function(object,status,start=NULL,end=NULL,type="",YCS=1,use_F=TRUE,F_guess=NULL,catch_guess=NULL,DELTA=10e-3){
            # Searching for a constant F or catch that would fish the population down to status of B0 from start to end
            # or until reaching equilibrium if start and end is not defined or  end = Inf 
            if(is.null(object@annual_cycle)) stop("annual_cycle not defined!")
            if(is.null(start) & is.null(end)){
                option="equilibrium"
            } else {
                if(is.null(start) | is.null(end)) stop("start or end is null")
                if(start > end) stop("start > end!")
                if(!is.finite(end)) option <- "equilibrium"
                else option <- ""
            }
            if(option!="equilibrium" & type!="constant"){
                if(length(YCS) != end-start+1) stop("the length of YCS should be equal to end-start+1")
            } else if(option!="equilibrium" & type=="constant") {
                YCS <- rep(1,end-start+1)
            }
            if(!is.null(F_guess) & !is.null(catch_guess)) stop("use either catch _guess or F_guess, but not both!")            
            if(status > 1.00 || status < 0) stop("Current status should not be over 100% B0 or less than 0% B0")

            if(status==1){
                SSB <- object@SSB
                while(TRUE){
                    object <- annual_cycle(object,type="constant",YCS=1,F=0,catch=NULL)
                    if(abs(object@SSB-SSB) < DELTA) break
                    else SSB <- object@SSB
                }  
            } else if(use_F){
                if(is.null(F_guess)) F_guess <- object@F_max
                if(F_guess <= 0) stop("F_guess <=0!")
                pre_object <- object
                F_low <- 0
                F_high <- F_guess
                cat("testing F=")
                while(TRUE) {                    
                    cat(F_high," ")
                    if(option=="equilibrium"){
                        SSB <- object@SSB
                        while(TRUE) {
                            object <- annual_cycle(object,type="constant",YCS=1,F=F_high,catch=NULL)
                            if(abs(object@SSB-SSB) <= DELTA) break
                            else SSB <- object@SSB
                        }
                    } else {
                        for (i in 1:(end-start+1)) {
                            object <- annual_cycle(object,type=type,YCS=YCS[i],F=F_high,catch=NULL)
                         }                    
                    }
                    if((object@SSB > object@B0*status) > DELTA){
                            object <- pre_object
                            F_low <- F_high
                            F_high <- 2*F_high
                            if(F_high*max(get_selectivities(object@fishing_selectivity)) > object@F_max) stop(paste("tesing F=",F_high," exceeded F_max",sep=""))
                    } else break
                }
                if(abs(object@SSB-object@B0*status) > DELTA){
                    object <- pre_object                      
                    while(TRUE){
                        cat((F_high+F_low)/2," ")
                        if(option=="equilibrium"){
                            SSB <- object@SSB
                            while(TRUE){
                                object <- annual_cycle(object,type="constant",YCS=1,F=(F_high+F_low)/2,catch=NULL)
                                if(abs(object@SSB-SSB) <= DELTA) break
                                else SSB <- object@SSB
                            }
                         } else {
                             for (i in 1:(end-start+1)) {
                                 object <- annual_cycle(object,type=type,YCS=YCS[i],F=(F_high+F_low)/2,catch=NULL)
                             }                    
                         }        
                         if(abs(object@SSB-object@B0*status) < DELTA) break
                         else {
                             if(object@SSB > object@B0*status){
                                 F_high <- F_high
                                 F_low <- (F_high+F_low)/2
                             } else {
                                 F_low <- F_low
                                 F_high <- (F_high+F_low)/2
                             }
                             if(abs(object@F-(F_high+F_low)/2) <DELTA) break # Add this to reduce time
                             object <- pre_object
                         }
                    }
                }
               cat("\n")
            } else {
                if(is.null(catch_guess)) catch_guess <-  sum(get_row(object)*get_mean_weight(object@mean_weight)*(1-exp(-object@F_max*get_selectivities(object@fishing_selectivity))))
                if(catch_guess <= 0) stop("catch_guess <=0!")
                pre_object <- object
                catch_low <- 0
                catch_high <- catch_guess
                cat("testing catch=")
                while(TRUE) {                    
                    cat(catch_high," ")
                    if(option=="equilibrium"){
                        SSB <- object@SSB
                        while(TRUE) {
                            object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=catch_high)
                            if(abs(object@SSB-SSB) <= DELTA) break
                            else SSB <- object@SSB
                        }
                    } else {
                        for (i in 1:(start-end+1)) {
                            object <- annual_cycle(object,type=type,YCS=YCS[i],F=NULL,catch=catch_high)
                         }                    
                    }
                    if((object@SSB > object@B0*status) > DELTA){
                            object <- pre_object
                            catch_low <- catch_high
                            catch_high <- 2*catch_high
                            if(max(catch_high/sum(get_row(object)*get_selectivities(object@fishing_selectivity)*get_mean_weight(object@mean_weight))*
                                   get_selectivities(object@fishing_selectivity)) > object@U_max)
                               stop(paste("tesing catch=",catch_high," exceeded U_max",sep=""))
                    } else break
                }
                if(abs(object@SSB-object@B0*status) > DELTA){
                    object <- pre_object                      
                    while(TRUE){
                        cat((catch_high+catch_low)/2," ")
                        if(option=="equilibrium"){
                            SSB <- object@SSB
                            while(TRUE){
                                object <- annual_cycle(object,type="constant",YCS=1,F=NULL,catch=(catch_high+catch_low)/2)
                                if(abs(object@SSB-SSB) <= DELTA) break
                                else SSB <- object@SSB
                            }
                         } else {
                             for (i in 1:(start-end+1)) {
                                 object <- annual_cycle(object,type=type,YCS=YCS[i],F=NULL,catch=(catch_high+catch_low)/2)
                             }                    
                         }        
                         if(abs(object@SSB-object@B0*status) < DELTA) break
                         else {
                             if(object@SSB > object@B0*status){
                                 catch_high <- catch_high
                                 catch_low <- (catch_high+catch_low)/2
                             } else {
                                 catch_low <- catch_low
                                 catch_high <- (catch_high+catch_low)/2
                             }
                             object <- pre_object
                         }
                    }
                }                
               cat("\n") 
            }
            return(object)
        }
)




setMethod("recruitment_process", signature(object="State"),
	function(object,type="",YCS=1){
            if(type=="constant"){
                 recruits <- object@R0
            } else {
                 if(is.null(YCS)) stop("YCS is null")
                 #if(YCS <=0.1 || YCS > 10) stop(paste("YCS of ",YCS,", too extreme!",sep=""))
                 recruits <- object@R0 * YCS
                 if(!is.null(object@stock_recruit))
                 recruits <- recruits * SR(object@stock_recruit,object@SSB)
            }
            object@.Data <- object@.Data+recruits*object@initial_size_dist
            return(object)
	}
)

setMethod("growth_process", signature(object="State"),
	function(object){
            object@.Data = object@.Data %*% object@growth@transition_matrix
          return(object)
	}            
)  

setMethod("M_process", signature(object="State"),
	function(object,fraction=1){
            if(fraction < 0 || fraction > 1) stop("fraction must be between 0 and 1") 
            if(fraction==1)
                object@.Data = object@.Data*get_exp_minus_M_by_element(object@natural_mortality)
            else
                object@.Data = object@.Data*exp(-get_M_by_element(object@natural_mortality)*fraction)
          return(object)
	}            
)

setMethod("spawning_process", signature(object="State"),
	function(object,premortality_partition,proportion=0.5,method="weighted_sum"){
            partmortality_partition <- average_partition(object,premortality_partition,proportion=proportion,method=method)
            object@SSB = sum(partmortality_partition*get_mean_weight(object@mean_weight)*get_mature_proportions(object@maturity))
          return(object)
	}            
)  

setMethod("catch_process", signature(object="State"),
	function(object,catch){
            if(catch < 0) stop("catch < 0")
            U <- catch/sum(get_row(object)*get_selectivities(object@fishing_selectivity)*get_mean_weight(object@mean_weight))
            U_obs <- max(U*get_selectivities(object@fishing_selectivity))
            fishing_pressure_limit_exceeded <- FALSE
            if (U_obs > object@U_max) {
                U <- U*object@U_max/U_obs
                U_obs <- max(U*get_selectivities(object@fishing_selectivity))
                fishing_pressure_limit_exceeded <- TRUE
            } 
            object@U <- U
            object@U_obs <- U_obs
            object@catch <- catch
            object@actual_catch <- sum(U*object@.Data*get_selectivities(object@fishing_selectivity)*get_mean_weight(object@mean_weight))
            object@fishing_pressure_limit_exceeded <- fishing_pressure_limit_exceeded
            # F cannot be determined analytically
            object@.Data <- object@.Data*(1-object@U*get_selectivities(object@fishing_selectivity))
          return(object)
	}            
)  

setMethod("F_process", signature(object="State"),
	function(object,F){
            if(F < 0) stop("F < 0")
            catch <- sum(get_row(object)*get_mean_weight(object@mean_weight)*(1-exp(-F*get_selectivities(object@fishing_selectivity))))
            F_obs <- F*max(get_selectivities(object@fishing_selectivity))
            fishing_pressure_limit_exceeded <- FALSE
            if (F_obs > object@F_max) {
                F <- object@F_max/max(get_selectivities(object@fishing_selectivity))
                F_obs <- F*max(get_selectivities(object@fishing_selectivity))
                fishing_pressure_limit_exceeded <- TRUE
            }
            object@catch <- catch
            object@actual_catch <-  sum(get_row(object)*get_mean_weight(object@mean_weight)*(1-exp(-F*get_selectivities(object@fishing_selectivity))))
            object@F <- F
            object@F_obs <- F_obs
            object@fishing_pressure_limit_exceeded <- fishing_pressure_limit_exceeded
            # U can be calculated, but not for now

            object@.Data <- object@.Data*exp(-object@F*get_selectivities(object@fishing_selectivity))
            object@SSB <- 
          return(object)
	}            
)  




setMethod("YPR_SPR",signature(object="State"),
		function(object,F_low=0.01,F_high=0.99,F_step=0.01,DELTA=10e-3,YPR=FALSE,SPR=FALSE){
			if(is.null(object@annual_cycle)) stop("annual_cycle not defined!")
			if(F_low < 0)  stop("F_low <=0!")
			if(F_high > object@F_max)  stop("F_high > object@F_max!")
			if(F_low > F_high) stop ("F_low < F_high")Abalone 
			
			pre_object <- object
			catch <- c()
			SSB <- c()
			for(F in seq(F_low,F_high,by=F_step)) {
				cat("testing F=",F,"\n")
				last_SSB <- object@SSB
				while(TRUE) {
					object <- annual_cycle(object,YCS=1,F=F,catch=NULL)
					if(abs(object@SSB-last_SSB) <= DELTA) break
					else last_SSB <- object@SSB
				}
				catch =c(catch,object@catch)
				SSB = c(SSB,object@SSB)
				object = pre_object 
			}
			if(YPR) catch = catch/object@R0
			if(SPR) SSB = SSB/object@B0
            return(list(F=seq(F_low,F_high,by=F_step),YPR=catch,SPR=SSB))
		} )



setMethod("deterministic_MSY",signature(object="State"),
		function(object,F_low=0.01,F_high=0.99,F_step=0.01,DELTA=10e-3){
			if(is.null(object@annual_cycle)) stop("annual_cycle not defined!")
			if(F_low < 0)  stop("F_low <=0!")
			if(F_high > object@F_max)  stop("F_high > object@F_max!")
			if(F_low > F_high) stop ("F_low < F_high")
			
			pre_object <- object
			catch <- c()
			SSB <- c() 
                        for(F in seq(F_low,F_high,by=F_step)) {
				cat("testing F=",F,"\n")
				last_SSB <- object@SSB
				while(TRUE) {
					object <- annual_cycle(object,YCS=1,F=F,catch=NULL)
					if(abs(object@SSB-last_SSB) <= DELTA) break
					else last_SSB <- object@SSB
				}
				catch =c(catch,object@catch)
				SSB = c(SSB,object@SSB)
				object = pre_object 
                        }
                        index = which.max(catch)
                        
			
            return(list(FMSY=seq(F_low,F_high,by=F_step)[index],MSY=catch[index],BMSY=SSB[index]))
		} )







