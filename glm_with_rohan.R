install.packages("glmm")

x = runif(100, 0,3)
b0 = runif(1)
b = runif(1)
y = rpois(100,exp(b0 + b*x))
?rpois
data = data.frame(x,y)
?glm
l = numeric()
l0 = numeric()
g1 = glm(y ~ x,family = poisson,data= data)
fm = y ~ l0 + x*l
class(fm)
g2 = glm.fit(x,y)
coefficients(g2)
