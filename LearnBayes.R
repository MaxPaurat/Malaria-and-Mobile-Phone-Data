# Learning Bayes

p = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
plot(p, prior, type = "h", ylab="Prior Probability")


library(LearnBayes)
data = c(11, 16)
post = pdisc(p, prior, data)
plot(p, post, type = "h", ylab="Prior Probability")

library(lattice)
PRIOR=data.frame("prior",p,prior)
POST=data.frame("posterior",p,post)
names(PRIOR)=c("Type","P","Probability")
names(POST)=c("Type","P","Probability")
data=rbind(PRIOR,POST)

xyplot(Probability~P|Type,data=data,layout=c(1,2), type="h",lwd=3,col="black")

quantile2=list(p=.9,x=.5)
quantile1=list(p=.5,x=.3)
beta.select(quantile1,quantile2)

a = 3.26
b = 7.19
s = 11
f = 16
curve(dbeta(x,a+s,b+f), from=0, to=1,xlab="p",ylab="Density",lty=1,lwd=4)
curve(dbeta(x,s+1,f+1),add=TRUE,lty=2,lwd=4)
curve(dbeta(x,a,b),add=TRUE,lty=3,lwd=4)
legend(.7,4,c("Prior","Likelihood","Posterior"),lty=c(3,2,1),lwd=c(3,3,3))

1 - pbeta(0.5, a + s, b + f)

qbeta(c(0.05, 0.95), a + s, b + f)

ps = rbeta(1000, a + s, b + f)

hist(ps,xlab="p",main="")
sum(ps >= 0.5)/1000
quantile(ps, c(0.05, 0.95))

midpt = seq(0.05, 0.95, by = 0.1)
prior = c(1, 5.2, 8, 7.2, 4.6, 2.1, 0.7, 0.1, 0, 0)
prior = prior/sum(prior)
curve(histprior(x,midpt,prior), from=0, to=1,ylab="Prior density",ylim=c(0,.3))

curve(histprior(x,midpt,prior) * dbeta(x,s+1,f+1),from=0, to=1, ylab="Posterior density")


p = seq(0, 1, length=500)
post = histprior(p, midpt, prior) * dbeta(p, s+1, f+1)
post = post/sum(post)
ps = sample(p, replace = TRUE, prob = post)
hist(ps, xlab="p", main="")

