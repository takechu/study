# 最急降下法

library(ggplot2)

f  = function(x,y){ (x-1)^2+10*(x^2-y)^2 }
df = function(x,y){ c( 2*(x-1)+20*(x^2-y)*2*x,
                      -20*(x^2-y) ) }

# 関数値の可視化
N <- 100
x <- seq(-0.5,1.5, length=N)
y <- seq(-0.5,1.5, length=N)
d.cont <- data.frame( x=rep(x,N), y=rep(y,each=N) )
d.cont <- transform(d.cont, z=f(x,y) )

p <- ggplot()
p <- p + geom_tile( data=d.cont, aes(x=x,y=y,z=z,fill=log(z) )) 
p <- p + scale_fill_gradient(low = "red", high = "white") 
print(p)
                    
# 最急降下
x = 0
y = 1
as = seq(0,10,length.out = 10000)
ps = data.frame( x=x, y=y, z=f(x,y) )
while( sum(df(x,y)^2) > 0.0001 ){
  d = df( x, y )
  dx = d[1]
  dy = d[2]
  # 直線探索
  a = as[ which.min( f(x-as*dx,y-as*dy) ) ]
  x = x - a*dx
  y = y - a*dy
  ps = rbind( ps, data.frame( x=x, y=y, z=f(x,y) ) )
}

p <- ggplot()
p <- p + geom_tile( data=d.cont, aes(x=x,y=y,z=z,fill=log(z) )) 
p <- p + scale_fill_gradient(low = "red", high = "white") 
p <- p + geom_point( data=ps, aes(x=x,y=y) ) + geom_path( data=ps, aes(x=x,y=y) )
print(p)
