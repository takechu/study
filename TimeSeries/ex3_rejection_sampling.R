# 棄却サンプリング（複雑な確率分布からのサンプリング）
#
# ある分布p(x)からサンプリングする
#  仮定１ 直接サンプリングは困難
#  仮定２ あるxに対するp(x)の計算は正規化定数以外は簡単にできる
#
# p(x) : ここではガンマ分布Gam(x|a,b)とする
#  p(x)の値はdgamma(x,a,b)で計算可能
#

library(ggplot2)
library(reshape2)
N=1000

# 提案分布：コーシー分布 q(x)=dcauchy(x,9,sqrt(19))
#  条件 すべてのxに対してk*q(x)>=p(x)
#  チェック
x = seq(0,30,0.1)
k = 2.0
plot.data = data.frame( x=x, p_x=dgamma(x,10), kq_x=k*dcauchy(x,9,sqrt(19)))
gp = ggplot( melt( plot.data, id.vars="x" ) )
gp = gp + geom_line( aes(x=x,y=value,group=variable,color=variable))
print( gp )

# ステップ１：提案分布q(x)からx_をサンプリング
sample <- rcauchy(N,9,sqrt(19))
# ステップ２：一様分布[0,q(x_)]からu_をサンプリング
u <- runif(N,min=0,max=1)*dcauchy(sample,9,sqrt(19))
# ステップ３：p(x_)<u_ ならサンプルを棄却
is_reject <- u > dgamma( sample, 10 )

# 可視化
plot.data2 = rbind( data.frame( step="proposed", x=sample ),
                    data.frame( step="accepted", x=sample[which(!is_reject)]) )
plot.data2$step = factor( plot.data2$step, levels = c("accepted","proposed"))
gp = ggplot( plot.data2 )
gp = gp + geom_histogram( aes( x=x, fill=step, y=..density.. ),
                          position="identity", binwidth=0.5, alpha=0.3, )
gp = gp + geom_line( data = melt( plot.data, id.vars="x"),
                     aes(x=x,y=value,group=variable,color=variable))
gp = gp + coord_cartesian( xlim=c(0,30) )
print( gp )
