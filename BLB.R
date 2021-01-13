N=10^6
U =runif(N)                   
pop = rep(NA,N)
for(i in 1:N){
  if(U[i]<.3){
    pop[i] = rnorm(1,0,1)
  }
  else{
    pop[i] = rnorm(1,4,2)
  }}

n=.1*N
x = sample(pop,n)
ggplot()+geom_histogram(aes(x,..density..),fill='gray',col='black') + 
  geom_density(aes(pop),col='red',size=1)  + theme_bw()



boot = foreach(i = 1:1000,.combine = 'c') %dopar% {
  median(sample(x,n,replace = TRUE))
}

mm = function(x,n,b){
  return(rmultinom(b,n,1/b)/n * sample(x,b,replace = TRUE))
}
BLB = function(x,gamma,r,s,stat,metric,vec=FALSE,...){
  n = length(x)
  b = floor(n^gamma)
  x_s = split(sample(x,s*b), ceiling(seq_len(s*b)/b))
  ep = foreach(j = x_s) %do% {
    metric(
      foreach(i = 1:r,.combine = 'c') %dopar% {
      stat(mm(j,n,b))
    },...)
  }
  
  if(vec==TRUE){
    ep = unlist(ep)
  }
  return(ep)
}
suma = function(x){
  return(c(mean=mean(x),quantile(x,probs=c(.025,.975))))
}

s=50
gamma=.65 # b=1000
r=100
n^gamma * s < n
library(parallel)
cl = parallel::makeCluster(10)


blb = do.call('rbind',BLB(pop,gamma,r = r,s = s,median,suma))
stopCluster(cl)
median(pop)
mean(boot)
quantile(boot,probs = c(.025,.975))
apply(blb, 2,mean)


library(microbenchmark)

meth_time = microbenchmark(
  
  BOOT = {
    boot = foreach(i = 1:1000) %dopar% {
    median(sample(x,n,replace = TRUE))
  }},
  BLB = {
    BLB(pop,gamma,r = r,s = s,median,suma)
  },
  times=25
    
  
  
)
autoplot(meth_time)












library(tidyverse)

ggplot()+geom_histogram(aes(boot,..density..),fill='gray',col='black') + theme_classic()+ 
  geom_vline(aes(xintercept = mean(boot)),col='firebrick2',size=1)+
  geom_vline(aes(xintercept = median(pop)),col='dodgerblue',size=1)+
  geom_vline(aes(xintercept = quantile(boot,.025)),col='orange',size=1)+
  geom_vline(aes(xintercept = quantile(boot,.975)),col='orange',size=1)











