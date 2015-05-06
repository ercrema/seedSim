#pooled sd function (needs number and sd vectors)
pooledSd <- function(n, d) 
{
    return(sqrt(sum((n-1) * d^2) / sum(n - 1)))
}

#must be "b" estimated with the real data for each case?, i.e. the value which generate the observed size difference
simSeed<-function(N=100,n=10,b=0.005,ngen=1000,selq=0.2,sigma=1,samplesize=5) 
{
    require(VGAM)
    
                                        #Initialise Population
    genotype=matrix(NA,nrow=N,ncol=3) #genotype matrix
    genotype[,1]=rnorm(N,50,sigma) #first column contains randomly initialised means
    genotype[,2]=sigma #second column contains the standard deviation
    genotype[,3]=n #third column contains the number of offsprings

                                        # Define Archaeological Record
    #record=matrix(NA,nrow=ngen,ncol=samplesize)
    recordedSDs<-numeric()
    recordedAvgs<-numeric()
                                        #Start Simulation

    for (t in 1:ngen) #or 1:max(timeTable)
        {
                                        #generate seeds
            seeds=as.numeric(t(apply(genotype,1,function(x){replicate(x[3],rnorm(1,x[1],x[2]))})))
            index=rep(1:N,n) #record pointer to parent matrix

                                        #record 
            recordedSDs[t]=sd(seeds) #sd
            recordedAvgs[t]=mean(seeds) #mean

                                        ##random selection (archaeological sampling)
            #sample.index=sample(1:length(seeds),size=samplesize,replace=FALSE)
            #record[t,]=seeds[sample.index]
            #seeds=seeds[-sample.index] #remove from sample pool
            #index=index[-saMple.index] #remove from sample pool

                                        #anthrophic selection
            
            threshold=quantile(seeds,prob=selq) #define selection threshold, i.e. only seeds bigger than this value will be selected 

            sel=which(seeds<threshold) #need to write a small function to avoid getting less seeds than N
            seeds=seeds[-sel] #remove from sample pool
            index=index[-sel] #remove from sample pool
            sowdIndex=index[sample(1:length(seeds),size=N,replace=FALSE)] #selected seeds' parent index (need to 

            genotype=genotype[sowdIndex,] #create new genotype
            
                                        #Mutation
            genotype[,1]=genotype[,1]+rlaplace(nrow(genotype),0,scale=b) #mutation affected all gens (not anymore)
        }

                                        #Holder Calculate Haldane (without collapse)

    haldanes=recordedAvgs/pooledSd(rep(n*N,ngen),recordedSDs) #dependent variable (y axis in the linear model)
    Time=1:ngen #independent variable (x axis in the linear model)
    haldaneCoef=summary(lm(haldanes~Time))$coefficients[2] # B coefficient (haldanes rate)


    return(data.frame(haldane=haldaneCoef, N=N, selq=selq)) # return(list(raw=record,haldane=haldane))
}

                                        ### Collapse
#in order to callpase the generations we have to define a arqueological record: a random sampling (if it is possible, with a 
#different samplesize for each generation)

                                        ### Main function
#estimated execution time (CPU 2.93GHz DDR3 166 MHz): 90 min
mainFunction <- function(ngen=2000, settings=data.frame(N=c(rep(50, 9), rep(100, 9), rep(150, 9)), selq=c(rep(seq(0.1, 0.9, 0.1), 3))), sims=10)
{
    require(ggplot2)

    # define the result
    Result <- data.frame()

    # loop through simulations
    for (i in 1:sims) 
    {
        results <- Reduce(function(...) rbind(...), apply(settings, 1, function(x) {simSeed(N=x[1], selq=x[2], ngen=ngen)}))
        Result <- rbind(Result, results)
    }

    # ggplot
    p <- ggplot(Result, aes(x=selq, y=haldane, group=N)) + 
        #geom_point(aes(colour=factor(N)), size=1.5) + 
        geom_smooth(aes(colour=factor(N))) + 
        geom_hline(yintercept = 0.001)

    return(p)
}








