# MCMC（マルコフ連鎖モンテカルロ）：ハミルトニアンモンテカルロ
#
# やること：メトロポリス法に勾配情報も利用する
# 　　　　　運動量を表す補助変数を追加し、ハミルトニアン（全エネルギー）を使ってサンプルする
# 解釈：    Metropolis-Hastings(MH)の特殊な場合
#           確率分布が指数分布族である場合に使われる？
# 　　　　　サンプルが従う結合分布は補助変数と元の変数とが独立なので、元変数だけ使えばよい
#
# p(x) : ２次元ガウス分布N(x|u=[1.5,1.5],S=[1 0.8 0.8 1])とする
#  == p(x) = exp( -0.5*(x-u)'*inv(S)*(x-u) ) / Z := exp( -E(x) ) / Z
# E(x) : 0.5*(x-u)'*inv(S)*(x-u) = 0.5*(x-u)'*A*(x-u)
# ∂E(x)/∂x : 0.5*(inv(S)+inv(S)')*x = inv(S)*x

library(ggplot2)
library(reshape2)
N = 100
L = 100
u = c(1.5,1.5)
S = matrix( c(1,0.8,0.8,1), 2, 2 )
A = solve(S)
rho = 0.01

# 提案分布 q((x,y)|(x',y')) : q(x|(x',y'))=p(x|y') or q(y|(x',y'))=p(y|x')
# 　多次元正規分布の場合は、
#      p(x|y)=N(u_x+S_xy*S_x/S_y(y-u_y),sqrt(1-S_xy^2)*S_x)
#      p(y|x)=N(u_y+S_xy*S_y/S_x(x-u_x),sqrt(1-S_xy^2)*S_y)
x_ <- c(0,0)
p_ <- rnorm(2,0,1)  
sample_proposed <- matrix( NA, N, 2 )
sample_accepted <- matrix( NA, N, 2 )
is_reject <- rep( NA, N )

for( i in 1:N ){
  sample_accepted[i,] <- x_  

  H_ = 0.5 * t(x_-u) %*% A %*% (x_-u) + 0.5 * t(p_) %*% p_
  p_x_ = exp( -H_ )

  # ここからダイナミクスに従い提案
  p <- rnorm(2,0,1)  
  x <- x_ + 0.5 * rho * p
  for( l in 1:L ){
    x <- x + rho * p
    p <- p - rho * A %*% (x-u)
  }
  x <- x + 0.5 * rho * p
    
  H = 0.5 * t(x-u) %*% A %*% (x-u) + 0.5 * t(p) %*% p
  p_x = exp( -H )
  if( runif(1) < min(1,p_x/p_x_) ){
    x_ <- x
    p_ <- p
    is_reject[i] <- FALSE  
  } else {
    x_ <- x_
    p_ <- p_
    is_reject[i] <- TRUE
  }
  sample_proposed[i,] <- x
}
sample_accepted[i,] <- x_  

plot.data = data.frame( t=1:N, 
                        accepted_x=sample_accepted[,1], accepted_y=sample_accepted[,2],
                        proposed_x=sample_proposed[,1], proposed_y=sample_proposed[,2],
                        is_reject=ifelse(is_reject,"reject","accept") )
gp = ggplot( plot.data )
gp = gp + geom_point( aes(x=proposed_x,y=proposed_y,color=is_reject))
gp = gp + geom_segment( aes(x=accepted_x, xend=proposed_x,
                            y=accepted_y, yend=proposed_y, color=is_reject))
print( gp )

