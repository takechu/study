# MCMC（マルコフ連鎖モンテカルロ）：Metropolisアルゴリズム
#
# 棄却サンプリング（や重点サンプリング）がうまく働かない高次元空間でも
# うまくサンプリングする
#  仮定１ あるxに対するp(x)の計算は正規化定数以外は簡単にできる
#  ポイント１ 各サンプルは独立ではなく、マルコフ連鎖をなす
#  ポイント２ 提案分布は直前のサンプルを用いる
#  ポイント３ 直前のサンプルとの確率の比によって棄却or採用する
#
# p(x) : ２次元ガウス分布N(x|u=[1.5,1.5],S=[1 0.8 0.8 1])とする
#

library(mvtnorm)
library(ggplot2)
library(reshape2)
N = 100
u_p = c(1.5,1.5)
S_p = matrix( c(1,0.8,0.8,1), 2, 2 )
S_q = diag(0.2,2)

# 提案分布 q(x|x') : 直前のサンプルx'を中心とした等方２次元ガウス分布 N(x|u=x',S=0.2)
# 初期値：x_ = [0,0]
x_ <- c(0,0)
sample_accepted <- matrix( NA, N, 2 )
sample_proposed <- matrix( NA, N, 2 )
is_reject <- rep( NA, N )
for( i in 1:N ){
  sample_accepted[i,] <- x_
  x <- rmvnorm(1,x_,S_q)
  sample_proposed[i,] <- x
  p_x  <- dmvnorm(x, u_p,S_p)
  p_x_ <- dmvnorm(x_,u_p,S_p)
  # ここでp_x,p_x_は、比をとるので正規化前でよい
  # Metropolis-Hastingsアルゴリズムだと、p(x)/p(x_) ではなく、
  # (p(x)*q(x_|x)) / (p(x_)*q(x|x_)) で棄却or採用を判断する
  if( runif(1)<min(1,p_x/p_x_) ){
    x_ <- x
    is_reject[i] <- FALSE
  } else {
    x_ <- x_
    is_reject[i] <- TRUE
  }
}

plot.data = data.frame( t=1:N, 
                        accepted_x=sample_accepted[,1], accepted_y=sample_accepted[,2],
                        proposed_x=sample_proposed[,1], proposed_y=sample_proposed[,2],
                        is_reject=ifelse(is_reject,"reject","accept") )
gp = ggplot( plot.data )
gp = gp + geom_point( aes(x=proposed_x,y=proposed_y,color=is_reject))
gp = gp + geom_segment( aes(x=accepted_x, xend=proposed_x,
                            y=accepted_y, yend=proposed_y, color=is_reject))
print( gp )
