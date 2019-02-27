library(ggplot2) 
library(gridExtra)

lambda = 0.2
n = 40
nsim = 1000
mns = NULL
set.seed(2019)
for (i in 1 : nsim) {
        mns = c(mns, mean(rexp(n, lambda)))
        }

## theoretical mean
1/lambda

## mean
mean(mns)

## plot-mean
plotmean <- ggplot(mapping = aes(mns)) + 
        geom_histogram(alpha=0.3, fill='white', colour='black')
plotmean <- plotmean + geom_vline(xintercept = 1/lambda, 
        linetype="dashed", color = "red")
plotmean <- plotmean + geom_vline(xintercept = mean(mns), color = "blue")
plotmean <- plotmean + labs(title = "Distribution of sample means of exponential function (1000 simulations)",
        x="sample means")

plotmean

## theoretical variance
1/(lambda^2)/n


## variance
var(mns)


##random
plotrand <- ggplot(mapping = aes(rexp(nsim,lambda)) 
                + geom_histogram(alpha=0.3, fill='white', colour='black'))
                + labs(title = "Distribution randomized exponential function values (1000 simulations)", 
                        x="values")

## plot-norm: construct plot with norm, mean = theoretical mean, sd = theoretical sd
plotnorm <- plotmean + stat_function(fun = dnorm, 
        args = list(mean = 1/lambda, sd = sqrt(1/(lambda^2)/n)))

grid.arrange(plotrand, plotnorm, nrow = 2)


## part2
data(ToothGrowth)
str(ToothGrowth)

summary(ToothGrowth)

qplot(supp,len,data=ToothGrowth, facets=~dose, 
        main="Growth of guinea pigs teeth by supplement type and dosage (mg)",
        xlab="Supplement", ylab="Tooth length") + 
        geom_boxplot(aes(fill = supp))

