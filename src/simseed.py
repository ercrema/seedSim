#!/usr/bin/python
# -*- encoding: utf-8 -*-

import random
import numpy
from scipy import stats

##############################################################################

# seed object
class seed:
    def  __init__(self, m, s, n, b):
        self._phenotype = random.gauss(m, s) # normal  
        self._genotypeM = m
        self._genotypeS = s
        self._genotypeN = n
        self._mutationB = b
    # reproduction
    def reproduce(self):
        mutation = numpy.random.laplace(0, self._mutationB) # laplace
        # list comprehension
        descendence = [seed(self._genotypeM + mutation, 
                self._genotypeS, 
                self._genotypeN, 
                self._mutationB) for i in range(0, self._genotypeN)]
        return(descendence)

# community object
class community:
    def  __init__(self, N, m, s, n, b, selq):
        self._N = N # number 
        self._n = n # descendence
        self._selq = selq*100 # selection
        self._seeds = [seed(random.gauss(m, s), s, n, b) for i in range(0, N)]
        self._averages = []
        self._stderror = []
        self._haldanes = []
    # new generation
    def newGenration(self):
        # list comprehensions
        reproduction = [i.reproduce() for i in self._seeds] # lists list
        seedsPool = [i for j in reproduction for i in j] # unlist
        # create a phenotype list
        phenotypes = [i._phenotype for i in seedsPool]
        # store data
        self._averages.append(numpy.mean(phenotypes))
        self._stderror.append(numpy.std(phenotypes, ddof=1))
        # selection 
        treshold = numpy.percentile(phenotypes, self._selq)
        seedsPool = [i for i in seedsPool if i._phenotype > treshold]
        # new random seeds parents
        self._seeds = random.sample(seedsPool, self._N)
    # simulation
    def simulation(self, iterations, warmup):
        # newGenerations loop 
        for i in range(0, iterations + warmup):
            self.newGenration()
    # compute haldane ratio
    def haldane(self, iterations, warmup):
        number = [self._n * self._N for i in range(0, iterations)]
        pooledSd = (sum([(i-1)*(j**2) for (i, j) in zip(number, self._stderror[warmup:])]) / sum([i-1 for i in number]))**0.5
        haldaneR = [i/pooledSd for i in self._averages[warmup:]]
        haldaneC = stats.linregress(range(1, iterations+1), haldaneR)[0] #B coef
        return(haldaneC)

def main():
    #body -------

if __name__ == '__main__':
    main()


## example (to run in terminal)
a = community(100, 50, 1, 10, 0.005, 0.2)
a.simulation(1000, 100)
a.haldane(1000, 100)



