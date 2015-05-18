
settings <- data.frame(selq=seq(0.1, 0.8, by=0.01))
result <- apply(settings, 1, function(x) {simSeed(selq=x[1])})


    
result <- as.vector(result[700:1000,])
selq <- sort(rep(settings[,1], 301))

plot(selq, result, pch='.', col='blue')

