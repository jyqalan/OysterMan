posfun <- function(x,eps=0.05){
    ifelse(x>=eps,x,eps/(2-x/eps))
}


add.growth.VonB<-function(l=c(75,125),g=c(10,1),alpha,beta,delta,Len,type="basic",bar=T,...)
{

  if(type=="basic") {
    mu<-posfun(g[1]+(g[2]-g[1])*(Len-l[1])/(l[2]-l[1]))
    stdev <- alpha*mu^beta
    #lines(supsmu(Len,mu),...)
    lines(Len,mu,...)
    mu<-posfun(g[1]+(g[2]-g[1])*(Len-l[1])/(l[2]-l[1]))
    stdev <- alpha*mu^beta
    if(bar){
        segments(Len,mu+2*stdev,Len,mu-2*stdev,...)
        segments(Len-0.5,mu+2*stdev,Len+0.5,mu+2*stdev,...)
        segments(Len-0.5,mu-2*stdev,Len+0.5,mu-2*stdev,...)
    } else {
        lines(Len,mu+2*stdev,...)
        lines(Len,mu-2*stdev,...)
    }
  } else if(type=="exponential") {
    mu<-posfun(g[1]*(g[2]/g[1])^((Len-l[1])/(l[2]-l[1])))
    stdev <- alpha*mu^beta
    #lines(supsmu(Len,mu),...)
    lines(Len,mu,...)
    mu<-posfun(g[1]*(g[2]/g[1])^((Len-l[1])/(l[2]-l[1])))
    stdev<-alpha*mu^beta
    if(bar){
        segments(Len,mu+2*stdev,Len,mu-2*stdev,...)
        segments(Len-0.5,mu+2*stdev,Len+0.5,mu+2*stdev,...)
        segments(Len-0.5,mu-2*stdev,Len+0.5,mu-2*stdev,...)
    } else {
        lines(Len,mu+2*stdev,...)
        lines(Len,mu-2*stdev,...)
    }         
  } else {
    stop("Wrong type!")     
  }

  return(list(mu=mu,stdev=stdev))
}

growthFUN.VonB <- function(l=c(75,125),g=c(10,1),alpha,beta,delta,type="basic",Len,...){
   if(type=="basic") {
       gg = posfun(1+(g[1]-g[2])/(l[1]-l[2]));       
       mu <- posfun(((l[2]*g[1]-l[1]*g[2])/(g[1]-g[2])-Len)*(1-gg^delta))
       #mu<-posfun(g[1]+(g[2]-g[1])*(Len-l[1])/(l[2]-l[1]))
       stdev <- alpha*mu^beta
       return(list(mu=mu,stdev=stdev))
   } else if(type=="exponential") {
       mu<-posfun(delta*g[1]*(g[2]/g[1])^((Len-l[1])/(l[2]-l[1])))
       stdev <- alpha*mu^beta
       return(list(mu=mu,stdev=stdev))   
   } else {
       stop("Wrong type")
   }
}


calcG.VonB <- function(l=c(75,120),g=c(10,1),alpha,beta,delta,type="basic",Len,...){
    mu <- growthFUN.VonB(l,g,alpha,beta,delta,type,Len)$mu
    stdev <- growthFUN.VonB(l,g,alpha,beta,delta,type,Len)$stdev
    N <- length(Len)
    G <- matrix(0,N,N)
    for (from in 1:N)
    {
      sumprob <- 0;
      G[from,1] = pnorm(Len[1]-Len[from],mu[from],stdev[from]);
      sumprob=G[from,1];
      for (to in 2:(N-1))
      {
        G[from,to] = pnorm(Len[to]-Len[from],mu[from],stdev[from])-sumprob;
        sumprob <- sumprob+G[from,to];
      }
      G[from,N] = 1 - sumprob;
  }
  rownames(G) <- colnames(G) <- Len
  G
}

add.G<-function(G,scale=5,...){
    x <- as.numeric(rownames(G))
    x <- x[(1:length(x)) %% 5==1]
    for(i in x){
        g <- G[as.character(i),]
        g <- g/max(g)*scale
        y <- as.numeric(names(g))
        y <- y[g> 0.001]
        lines(rep(i,length(y-i)),y-i,...)
        lines(i+g[as.character(y)],y-i,...)
    }
}
