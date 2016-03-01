# モンテカルロ積分
#
# 円の面積を求める
# (正方形に対する円の面積の比を求める)
# p(x) : -1.0 < x1 < 1.0, -1.0 < x2 < 1.0 の一様分布
# h1(x) : いつも1
# h2(x) : xが円の中なら1、外なら0
# E[h2(x)/h1(x)] : 正方形に対する円の面積比の期待値
#  ~ sum_x'( h2(x')/h1(x') ) / N

library(ggplot2)
N=1000

R=1
sample <- cbind( runif(N,min=-1,max=1), runif(N,min=-1,max=1) )
h_x <- ifelse( (sample[,1]^2+sample[,2]^2) < R^2, 1, 0 )

plot.data = data.frame( x=sample[,1], y=sample[,2], circle=factor(h_x) )
gp = ggplot( plot.data )
gp = gp + geom_point( aes( x=x, y=y, col=circle ) )
gp = gp + geom_path( aes( x=x, y=y ), data.frame( x=cos(seq(0,2*pi,by=0.01)), y=sin(seq(0,2*pi,by=0.01))) )
print( gp )
print( sum( h_x )/N*4 )