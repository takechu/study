# 最急降下法、ニュートン法、の比較

library(ggplot2)

call.2 = function( fun, x, y ){ fun(cbind(x,y)) }
f  = function(x){
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  (x[,1]-1)^2+(x[,2]-2)^2
  }
c1 = function(x){
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  x[,1]^2+x[,2]^2-2
}
c2 = function(x){
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  -x[,1]+x[,2]
}
c3 = function(x){
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  -x[,2]
}

# 関数値の可視化
N <- 100
x <- seq( -1.5, 3, length=N)
y <- seq( -1.5, 3, length=N)
d.cont <- data.frame( x=rep(x,N), y=rep(y,each=N) )
d.cont <- transform( d.cont, z=call.2( f, x, y ) )

p <- ggplot()
p <- p + geom_tile( data=d.cont, aes(x=x,y=y,z=z,fill=log(z) )) 
p <- p + geom_path( data=data.frame(x=sqrt(2)*cos(seq(0,2*pi,length.out=100)),
                                    y=sqrt(2)*sin(seq(0,2*pi,length.out=100))),
                    aes(x=x,y=y) )
p <- p + geom_path( data=data.frame(x=x,y=x), aes(x=x,y=y) )
p <- p + geom_path( data=data.frame(x=x,y=x), aes(x=x,y=0) )
p <- p + scale_fill_gradient(low = "red", high = "white") 
print(p)

# 最急降下法
x = c(0,1)
as = seq(0,10,length.out = 10000)
ps.grad = data.frame( x=x[1], y=x[2], z=f(x) )
while( sum(df(x)^2) > 0.0001 ){
  d = df(x)
  # 直線探索
  a = as[ which.min( f(sweep(-1*as%*%d,2,x,FUN="+")) ) ]
  x = x - a*d
  ps.grad = rbind( ps.grad, data.frame( x=x[1], y=x[2], z=f(x) ) )
}

# ニュートン法
x = c(0,1)
ps.new = data.frame( x=x[1], y=x[2], z=f(x) )
while( sum(df(x)^2) > 0.0001 ){
  d = q.solve( matrix(x,1,2) )
  x = x + d
  ps.new = rbind( ps.new, data.frame( x=x[1], y=x[2], z=f(x) ) )
}

# BFGS法
x = c(0,1)
B = matrix(c(1,0,0,1),2,2)

ps.quasi = data.frame( x=x[1], y=x[2], z=f(x) )
as = seq(0,10,length.out = 10000)
while( sum(df(x)^2) > 0.0001 ){
  d = -1*df(x)%*%solve(B)
  a = as[ which.min( f(sweep(as%*%d,2,x,FUN="+")) ) ]
  s = a*d
  y = -1*df(x)
  x = x + a*d
  y = y + df(x)
  ps.quasi = rbind( ps.quasi, data.frame( x=x[1], y=x[2], z=f(x) ) )
  if( y%*%t(s)<0) next 
  B = B + 1.0 / as.numeric(y%*%t(s)) * (t(y)%*%y)- 1.0 / as.numeric(s%*%B%*%t(s)) * B %*% t(s) %*% s %*% B
}

data <- rbind( data.frame( method="gradient", iter=seq(1,nrow(ps.grad)), x=ps.grad$x, y=ps.grad$y, f=ps.grad$z ),
               data.frame( method="newton",   iter=seq(1,nrow(ps.new)),  x=ps.new$x,  y=ps.new$y,  f=ps.new$z ),
               data.frame( method="quasi",    iter=seq(1,nrow(ps.quasi)),x=ps.quasi$x,y=ps.quasi$y,f=ps.quasi$z ) )

# 収束の度合い
p <- ggplot()
p <- p + geom_line( data=data, aes(x=iter,y=f,group=method, color=method ) ) 
p <- p + scale_x_log10() + scale_y_log10()
print(p)

# 軌跡
p <- ggplot()
p <- p + geom_tile( data=d.cont, aes(x=x,y=y,z=z,fill=log(z) )) 
p <- p + scale_fill_gradient(low = "gray", high = "white") 
p <- p + geom_point( data=data, aes(x=x,y=y,color=method) ) + geom_path( data=data, aes(x=x,y=y,color=method) )
print(p)
