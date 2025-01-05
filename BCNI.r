simpson.simulator <- function(N,s,ce){
	Z1 <- rnorm(N,0,s)
	Z3 <- rnorm(N,0,s) + Z1
	Z5 <- rnorm(N,0,s) + Z3
	U <- rnorm(N,0,s) + Z1
	Z4 <- rnorm(N,0,s) + Z5 + U
	Z2 <- rnorm(N,0,s) + Z3 + U
	X <- rnorm(N,0,s) + U
	Y <- rnorm(N,0,s) + ce*X + 10*Z5
	data.frame(Y,X,Z1,Z2,Z3,Z4,Z5)
}

# 1st parameter: sample size
# 2nd parameter: noise standard deviation
# 3rd parameter: true causal effect
D <- simpson.simulator(1000,0.01,1)



# unadjusted estimate
m <- lm(D[,1:2])
summary(m)
ci <- confint(m,'X')


# Plot result
plot( coef(m)['X'], 1, yaxt='n', bty='n', 
  xlab='X coefficient', ylab='', xlim=c(-5,5), 
  panel.first=abline(v=1,col=2), pch=19 )
segments( x0=ci[1], x1=ci[2], y0=1, y1=1 )
