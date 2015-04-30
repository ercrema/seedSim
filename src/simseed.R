

simSeed<-function(N=100,n=10,mu=0.001,b=0.1,ngen=1000,selq=0.2,sigma=1,samplesize=5)
{
    require(VGAM)
                                        #Initialise Population
    genotype=matrix(NA,nrow=N,ncol=3) #genotype matrix
    genotype[,1]=runif(N,min=50,max=55) #first column contains randomly initialised means
    genotype[,2]=sigma #second column contains the standard deviation
    genotype[,3]=n #third column contains the number of offsprings

                                        # Define Archaeological Record

    record=matrix(NA,nrow=ngen,ncol=samplesize)
    
                                        #Start Simulation

    for (t in 1:ngen)
        {
                                        #generate seeds
            seeds=as.numeric(t(apply(genotype,1,function(x){replicate(x[3],rnorm(1,x[1],x[2]))})))
            index=rep(1:N,n) #record pointer to parent matrix


                                        #random selection (archaeological sampling)
            sample.index=sample(1:length(seeds),size=samplesize,replace=FALSE)
            record[t,]=seeds[sample.index]
            seeds=seeds[-sample.index] #remove from sample pool
            index=index[-sample.index] #remove from sample pool

                                        #anthrophic selection
            
            threshold=quantile(seeds,prob=selq) #define selection threshold, i.e. only seeds bigger than this value will be selected 

            sel=which(seeds<threshold) #need to write a small function to avoid getting less seeds than N
            seeds=seeds[-sel] #remove from sample pool
            index=index[-sel] #remove from sample pool
            sowdIndex=index[sample(1:length(seeds),size=N,replace=FALSE)] #selected seeds' parent index (need to 

            genotype=genotype[sowdIndex,] #create new genotype
            
                                        #Mutation
            mutationIndex=which(runif(N)<mu)
            if (length(mutationIndex)>0)
                {
                    genotype[mutationIndex]=genotype[mutationIndex]+rlaplace(length(mutationIndex),0,scale=b)
                }
        }

    return(record)

}