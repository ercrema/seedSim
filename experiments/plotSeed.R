
require(reshape)
require(ggplot2)

load('seedSimResult.RData')

m.result <- melt(result, id=c('b', 'N', 'S'))

# selected b or N
p <- ggplot(m.result[m.result$b == 0.005,], aes(x=S, y=value, group=N)) + 
	geom_smooth(aes(colour=factor(N))) + 
    geom_hline(yintercept = 0.001, linetype=2)

# interaction b - N
p <- ggplot(m.result, aes(x=S, y=value, group=interaction(b, N))) + 
	geom_smooth(aes(colour=factor(N), linetype=factor(b))) + 
    geom_hline(yintercept = 0.001, linetype=2)


