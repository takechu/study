# MCMC（マルコフ連鎖モンテカルロ）：過剰緩和法
#
# やること：ギブズサンプラーにおいて、変数自身以外の全条件付き分布(full conditional)がガウシアンの場合に用いる。
# 　　　　　変数を更新する際に、その変数の現在の値を考慮し、全条件付き分布の逆側に少し振る
#
# p(x) : ２次元ガウス分布N(x|u=[1.5,1.5],S=[1 0.8 0.8 1])とする
#

library(ggplot2)
library(reshape2)
N = 300
u = c(1.5,1.5)
S = matrix( c(1,0.999,0.999,1), 2, 2 )
a = -0.5

x_ <- c(0,0)
gibbs_accepted <- matrix( NA, N, 2 )
gibbs_proposed <- matrix( NA, N, 2 )
for( i in 1:N ){
  gibbs_accepted[i,] <- x_
  if( i %% 2 == 1 ){
    v <- rnorm(1,0,1)
    m <- u[1]+S[1,2]*S[1,1]/S[2,2]*(x_[2]-u[2])
    s <- sqrt(1-S[1,2]^2)*S[1,1]
    x <- c( m + s*v, x_[2] )
  } else {
    v <- rnorm(1,0,1)
    m <- u[2]+S[2,1]*S[2,2]/S[1,1]*(x_[1]-u[1])
    s <- sqrt(1-S[2,1]^2)*S[2,2]
    x <- c( x_[1], m+s*v )
  }
  gibbs_proposed[i,] <- x
  x_ <- x
}

x_ <- c(0,0)
over_accepted <- matrix( NA, N, 2 )
over_proposed <- matrix( NA, N, 2 )
for( i in 1:N ){
  over_accepted[i,] <- x_
  if( i %% 2 == 1 ){
    v <- rnorm(1,0,1)
    m <- u[1]+S[1,2]*S[1,1]/S[2,2]*(x_[2]-u[2])
    s <- sqrt(1-S[1,2]^2)*S[1,1]
    x <- c( m + a*(x_[1]-m) + sqrt(1-a^2)*s*v, x_[2] )
  } else {
    v <- rnorm(1,0,1)
    m <- u[2]+S[2,1]*S[2,2]/S[1,1]*(x_[1]-u[1])
    s <- sqrt(1-S[2,1]^2)*S[2,2]
    x <- c( x_[1], m + a*(x_[2]-m) + sqrt(1-a^2)*s*v )
  }
  over_proposed[i,] <- x
  x_ <- x
}


plot.data = data.frame( t=1:N, 
                        accepted_x_gibbs=gibbs_accepted[,1], 
                        accepted_y_gibbs=gibbs_accepted[,2],
                        proposed_x_gibbs=gibbs_proposed[,1],
                        proposed_y_gibbs=gibbs_proposed[,2],
                        accepted_x_over=over_accepted[,1], 
                        accepted_y_over=over_accepted[,2],
                        proposed_x_over=over_proposed[,1],
                        proposed_y_over=over_proposed[,2] )

gp = ggplot( plot.data )
gp = gp + geom_point( aes(x=proposed_x_gibbs,y=proposed_y_gibbs), col="black")
gp = gp + geom_point( aes(x=proposed_x_over,y=proposed_y_over), col="red")
#gp = gp + geom_segment( aes(x=accepted_x_over, xend=proposed_x_over,
#                            y=accepted_y_over, yend=proposed_y_over ))
print( gp )
