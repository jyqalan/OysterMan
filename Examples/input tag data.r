source("growth.r")

negLogLike <- function(p,l=c(75,125),variance_type="constant_variance",type="basic",data){
    if(variance_type=="constant_variance"){
        if(length(p) != 3) stop("expecting 4 parameters from p if type is 'constant_variance'")
        g <- c(p[1],p[2]);
        alpha <- p[3]
        beta <- 0
    } else {
        if(length(p) != 4) stop("expecting 5 parameters from p if type is not 'constant_variance'")
        g <- c(p[1],p[2]);
        alpha <- p[3]
        beta <- p[4]
    }
    Len <- data$Len
    delta <- data$dA
    y <- data$dL
    mu <- growthFUN.VonB(l,g,alpha,beta,delta,Len=Len,type=type)$mu
    stdev <- growthFUN.VonB(l,g,alpha,beta,delta,Len=Len,type=type)$stdev
    #Sum((y-mu)^2/(2*stdev^2)+0.5*log(2*pi) + log(stdev));
    -sum(dnorm(y,mu,stdev,log=T))
}


DATA <- read.csv("Tagging  PAU 4.csv",header=T)

###########
# Sandy Point Pitt Is
##########
data <- DATA[DATA$AREA=="Sandy Point Pitt Is.",]
a <- data.frame("L1"=data$LENGTH.1.1,"dL"=data$ANN.GROWTH.1,"dA"=1)
a$Len <- a$L1

# Assume constant standard deviation of expected growth at length (negative log likelihood method is the equivalent to the Excel least sqaure method)
res <- nlminb(start=c(20,5,0.2),objective=negLogLike,variance_type="constant_variance",type="basic",data=a)
windows(width=8.2,height=11.2)
par(xaxs="i",yaxs="i",las=1,mfrow=c(2,1),mar=c(4.5,4.5,1.5,1.5))
plot(a$L1,a$dL,type="n",xlab="Length class (mm)",ylab="Increment (mm)",xlim=c(65,155),ylim=c(-5,50))
points(jitter(a$L1,5),a$dL/a$dA,pch=16,cex=0.5,col="black")
abline(h=0)
add.growth.VonB(l=c(75,125),c(res$par[1],res$par[2]),alpha=res$par[3],beta=0,delta=1,Len=seq(70,150,by=10)+1,col="red")
G <- calcG.VonB(l=c(75,125),c(res$par[1],res$par[2]),alpha=res$par[3],beta=0,delta=1,Len=seq(70,180,by=1))
#add.G(G,scale=5,lty=2,col="red")

text(120,25,"Sandy Point Pitt Is",adj=0,cex=0.9)
text(120,22,paste("(","g75=",round(res$par[1],1),", ","g125=",round(res$par[2],1),")",sep=""),adj=0,cex=0.8)

source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\GenericFunctions.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Growth.R")
source("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\R-libraries\\OysterMan\\R\\Growth_basic.R")
growth_basic <- Growth_basic(n_classes=51,class_mins=seq(70,170,by=2),plus_group=T,plus_group_size=170,l1=75,l2=120,g1=25.25,g2=6.92,alpha=0.31,beta=1,minsigma=1.0)
G <- get_transition_matrix(growth_basic)
rownames(G) <- colnames(G) <- seq(70,170,by=2)
add.G(G,scale=5,lty=2,col="gray")


G <-  read.table("C:\\cygwin\\home\\fud\\modelling\\OysterMan\\casal\\G.txt",sep="\t",header=F)
G <- as.matrix(G)
rownames(G) <- colnames(G) <- seq(70,170,by=2)
add.G(G,scale=5,lty=2,col="blue")
