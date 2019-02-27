

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

bw = .1
library(ggplot2) 
library(gridExtra)

## plot-mean
plotmean <- ggplot(mapping = aes(mns)) + 
        geom_histogram(alpha=0.3, fill='white', colour='black', binwidth = bw)
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


setNames(data.frame(matrix(data = c(1/(lambda^2)/n,var(mns)), ncol = 2, nrow = 1)), 
        c("theoretical variance", "sample variance"))


##random
plotrand <- ggplot(mapping = aes(randsam)) +
                geom_histogram (alpha=0.3, fill='white', colour='black', binwidth = bw) +
        labs(title = "Distribution randomized exponential function values (1000 simulations)", 
                x="values") + xlim(0, 10)

## plot-norm: construct plot with norm, mean = theoretical mean, sd = theoretical sd

tmean = 1/lambda
tsd = sqrt(1/(lambda^2)/n)


plotnorm <- plotmean + stat_function (fun = function(x, mean, sd, n){
        n * dnorm(x = x, mean = mean, sd = sd) * bw
}, 
        args = c(mean = tmean, sd = tsd, n = nsim)) + xlim(0, 10)

grid.arrange(plotrand, plotnorm, nrow = 2)


## part2
data(ToothGrowth)
str(ToothGrowth)

summary(ToothGrowth)

qplot(supp,len,data=ToothGrowth, facets=~dose, 
        main="ToothGrowth data: length vs dose, given type of supplement",
        xlab="Supplement", ylab="Tooth length") + 
        geom_boxplot(aes(fill = supp))

qplot(len, data = ToothGrowth, geom = "histogram",facets = supp~dose, 
        fill = as.factor(dose)) + scale_fill_discrete( name = "Dose")

qqnorm(ToothGrowth$len);qqline(ToothGrowth$len)

tl <- split(ToothGrowth, ToothGrowth$dose)

testres = NULL
alpha = 0.05

for (i in c(1:3)) {
        testres[[i]] <- with(tl[[i]], t.test(len[supp == "OJ"], len[supp == "VC"],
                alternative = "g"))
}

results <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
        c("supp1", "supp2", "dose", "confint1", 
                "confint2", "p-value", "status"))
hypo <- function (t) {
        outZero <- sign(prod(t$conf.int))
        if ((outZero != 1) || (t$p.value > alpha)) {
                "fail to reject"
        } else {
                "reject"
        }
}


for (i in c(1:3)) {
        results[i,"supp1"] <- "OJ"
        results[i,"supp2"] <- "VC"
        results[i,"dose"] <- tl[[i]]$dose[1]
        results[i,"confint1"] <- testres[[i]]$conf.int[1]
        results[i,"confint2"] <- testres[[i]]$conf.int[2]
        results[i,"p-value"] <- testres[[i]]$p.value
        results[i,"status"] <- hypo(testres[[i]])
}

