b=c(0.005,0.05)
N=c(100,500,2500)
S=seq(0,0.8,0.1)
param=expand.grid(b=b,N=N,S=S)
cores=5
require(utils)
require(foreach)
require(doParallel)
registerDoParallel(cores=cores)

tmp=foreach(i=1:nrow(param),.combine=rbind) %dopar%
{
	print(i)
	replicate(50,simSeed(selq= param$S[i],N= param$N[i],b=param$b[i])$haldane)	
}
