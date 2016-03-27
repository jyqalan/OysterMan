source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\GenericFunctions.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Ogive.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Ogive_logistic.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Ogive_constant.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Selectivity.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Maturity.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Natural_mortality.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Growth.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Growth_basic.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Growth_exponential.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Growth_inverse_logistic.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Mean_weight.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Size_based_mean_weight.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Stock_recruit.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Ricker.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Beverton_Holt.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Partition.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\State.R")


############
### Constant
############
n_classes <- 51
class_mins <- seq(70, 170,by=2)
plus_group = T
plus_group_size = 170

initial_size_mean=75
initial_size_cv=0.025


B0 <- 1000000
U_max <- 0.8
F_max <- 2


fishing_selectivity=Selectivity(ogive=Ogive_logistic(low=1,high=n_classes,a50=126.26,a_to95=5.59),n_class=n_classes,class_mins=class_mins, plus_group=plus_group, plus_group_size=plus_group_size)
maturity=Maturity(ogive=Ogive_logistic(low=1,high=n_classes,a50=91.79,a_to95=19.58),n_class=n_classes,class_mins=class_mins, plus_group=plus_group, plus_group_size=plus_group_size)
natural_mortality=Natural_mortality(M=0.1,ogive=NULL,n_class=n_classes)
growth_basic <- Growth_basic(n_classes=n_classes,class_mins=class_mins,plus_group=plus_group,plus_group_size=plus_group_size,l1=75,l2=120,g1=25.25,g2=6.92,alpha=0.31,beta=1,minsigma=1.0)
mean_weight=Size_based_mean_weight(a=2.99e-08,b=3.303,n_classes=n_classes,class_mins=class_mins,plus_group=plus_group,plus_group_size=plus_group_size,)
stock_recruit=Beverton_Holt(h=0.75,B0=B0)  



initialisation_phase <- 120
exploitation_phase <- 30
projection_phase <- 50

seed <- 0


############
### Annual cycle function
############

do_year <- function(state,type="",YCS=1,F=NULL,catch=NULL){
     state <- recruitment_process(state,type=type,YCS=YCS)
     state <- growth_process(state)
     premortality_partition <- get_row(state)
     state <- M_process(state,fraction=0.5)
     if(!is.null(F) & !is.null(catch)) stop("use either catch or F in a year, but not both")
     if(!is.null(F)) state <- F_process(state,F=F)
     if(!is.null(catch)) state <- catch_process(state,catch=catch)
     state <- M_process(state,fraction=0.5)
     state <- spawning_process(state,premortality_partition)
     return(state)   
}



state=State(n_classes=n_classes,class_mins=class_mins,plus_group=plus_group,plus_group_size=plus_group_size,initial_size_mean=initial_size_mean,initial_size_cv=initial_size_cv,
           growth=growth_basic,maturity=maturity,natural_mortality=natural_mortality,fishing_selectivity=fishing_selectivity,mean_weight=mean_weight,stock_recruit= stock_recruit,
           U_max=U_max, F_max=F_max,annual_cycle=do_year)

state@R0 <- 1
state <- recruitment_process(state,type="constant")
state <- growth_process(state)

state <- set_initial_state(state,B0=B0,n_equilibrium=60)
state <- recruitment_process(state,type="constant")





library(casal)
source("C:\\development\\R\\casal\\CASAL.R")

mpd <-input.mpd("a.log","C:\\cygwin\\home\\fud\\modelling\\OysterMan\\casal\\")

