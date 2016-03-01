# モンテカルロ積分
#
# 重心を求める
# p(x) : N([1,1],[1 0.4 0.4 0.7]) + N([3,-1],[1 -0.7 -0.7 0.9])
# h(x) : x
# E[h(x)] : xの期待値＝重心？

library(ggplot2)
library(mvtnorm)
N=1000

sample <- rbind( rmvnorm( N/2, c(1,1),  matrix(c(1, 0.4, 0.4,0.7),2,2) ),
                 rmvnorm( N/2, c(3,-1), matrix(c(1,-0.7,-0.7,0.9),2,2) ) )
h_x <- sample
E_x <- apply(h_x,2,sum)/N
print( E_x )

plot.data = data.frame( x=sample[,1], y=sample[,2] )
gp = ggplot( plot.data )
gp = gp + geom_point( aes( x=x, y=y ) )
gp = gp + annotate( "point", x=E_x[1], y=E_x[2], col="red", size=5 )
print( gp )
