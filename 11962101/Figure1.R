# install.packages("ggplot2")
# install.packages("mvtnorm")
# library(ggplot2)
# library(mvtnorm)

# Left panel of Figure 1.

m1 <- c(0, 0)
m2 <- c(2,2)
sigma <- matrix(c(1,0,0,1), nrow=2)
data.grid <- expand.grid(s.1 = seq(-10, 10, length.out=200), s.2 = seq(-10, 10, length.out=200))


q1.samp = cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m1, sigma=sigma))
q2.samp = cbind(data.grid, prob = mvtnorm::dmvnorm(data.grid, mean = m2, sigma=sigma))

ggplot() + geom_contour(data=q1.samp,aes(x=s.1,y=s.2,z=prob))+geom_contour(data=q2.samp,aes(x=s.1,y=s.2,z=prob))+xlab("")+ylab("")+geom_abline(intercept=2, slope = -1)+geom_abline(intercept=2+0.5*log(0.05),slope=-1,color="orange",linetype=3,size=2)+geom_abline(intercept=2+0.5*log(0.5),slope=-1,color="red",linetype=3, size=2)+theme(text = element_text(size=20))

# Right panel of Figure 1.
x        = c(0,0.25,0.5,0.7,0.8,0.85,0.9,0.95,0.96)
y        = 1-pnorm(2/sqrt(2)+1/2/sqrt(2)*log(1-x))
data<-data.frame(x,y)


ggplot(data,aes(x=x, y=y))+geom_point(size = 4)+xlab("censorship rate")+ylab("Type I error")+geom_smooth(method = "loess", formula = y ~x, se = FALSE)+theme(text = element_text(size=20))