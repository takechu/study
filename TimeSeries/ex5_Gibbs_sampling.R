# MCMC（マルコフ連鎖モンテカルロ）：ギブズサンプリング
#
# やること：複数の変数のうち１つの変数を除いて固定し、その変数のアップデートを繰り返す
#           アップデートは、その変数自身以外の全条件付き分布(full conditional)より行う
# 解釈：    Metropolis-Hastings(MH)の特殊な場合
#           提案分布を、全条件付き分布とした場合、採用率は必ず１になる
#
# p(x) : ２次元ガウス分布N(x|u=[1.5,1.5],S=[1 0.8 0.8 1])とする
#

library(ggplot2)
library(reshape2)
N = 300
u = c(1.5,1.5)
S = matrix( c(1,0.8,0.8,1), 2, 2 )

# 提案分布 q((x,y)|(x',y')) : q(x|(x',y'))=p(x|y') or q(y|(x',y'))=p(y|x')
# 　多次元正規分布の場合は、
#      p(x|y)=N(u_x+S_xy*S_x/S_y(y-u_y),sqrt(1-S_xy^2)*S_x)
#      p(y|x)=N(u_y+S_xy*S_y/S_x(x-u_x),sqrt(1-S_xy^2)*S_y)
x_ <- c(0,0)
sample_accepted <- matrix( NA, N, 2 )
sample_proposed <- matrix( NA, N, 2 )
for( i in 1:N ){
  sample_accepted[i,] <- x_
  if( i %% 2 == 1 ){
    x <- c( rnorm( 1, u[1]+S[1,2]*S[1,1]/S[2,2]*(x_[2]-u[2]), sqrt(1-S[1,2]^2)*S[1,1]),
            x_[2] )
  } else {
    x <- c( x_[1],
            rnorm( 1, u[2]+S[2,1]*S[2,2]/S[1,1]*(x_[1]-u[1]), sqrt(1-S[2,1]^2)*S[2,2]) )
  }
  sample_proposed[i,] <- x
  x_ <- x
}

plot.data = data.frame( t=1:N, 
                        accepted_x=sample_accepted[,1], accepted_y=sample_accepted[,2],
                        proposed_x=sample_proposed[,1], proposed_y=sample_proposed[,2] )
gp = ggplot( plot.data )
gp = gp + geom_point( aes(x=proposed_x,y=proposed_y))
gp = gp + geom_segment( aes(x=accepted_x, xend=proposed_x,
                            y=accepted_y, yend=proposed_y ))
print( gp )
