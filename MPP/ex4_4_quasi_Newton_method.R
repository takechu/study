# 準ニュートン法

library(ggplot2)

call.2 = function( fun, x, y ){ fun(cbind(x,y)) }
f  = function(x){
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  (x[,1]-1)^2+10*(x[,1]^2-x[,2])^2
  }
df = function(x){
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  cbind( 2*(x[,1]-1)+20*(x[,1]^2-x[,2])*2*x[,1],
         -20*(x[,1]^2-x[,2] ) )
}
Hf = function(x){ 
  if( !is.matrix(x) ){ x = matrix(x,1,2) }
  matrix( c(2+40*(3*x[,1]^2-x[,2]),-40*x[,1],-40*x[,1],20), 2,2 )
  }

q.solve = function( x0 ){ -1 * df(x0) %*% solve(Hf(x0)) }

# 関数値の可視化
N <- 100
x <- seq(-0.5,1.5, length=N)
y <- seq(-0.5,1.5, length=N)
d.cont <- data.frame( x=rep(x,N), y=rep(y,each=N) )
d.cont <- transform(d.cont, z=call.2( f, x, y ) )

p <- ggplot()
p <- p + geom_tile( data=d.cont, aes(x=x,y=y,z=z,fill=log(z) )) 
p <- p + scale_fill_gradient(low = "red", high = "white") 
print(p)
                    
# BFGS法
x = c(0,1)
B = matrix(c(1,0,0,1),2,2)

ps = data.frame( x=x[1], y=x[2], z=f(x) )
as = seq(0,10,length.out = 10000)
while( sum(df(x)^2) > 0.0001 ){
  d = -1*df(x)%*%solve(B)
  a = as[ which.min( f(sweep(as%*%d,2,x,FUN="+")) ) ]
  s = a*d
  y = -1*df(x)
  x = x + a*d
  y = y + df(x)
  ps = rbind( ps, data.frame( x=x[1], y=x[2], z=f(x) ) )
  if( y%*%t(s)<0) next 
  B = B + 1.0 / as.numeric(y%*%t(s)) * (t(y)%*%y)- 1.0 / as.numeric(s%*%B%*%t(s)) * B %*% t(s) %*% s %*% B
}

p <- ggplot()
p <- p + geom_tile( data=d.cont, aes(x=x,y=y,z=z,fill=log(z) )) 
p <- p + scale_fill_gradient(low = "red", high = "white") 
p <- p + geom_point( data=ps, aes(x=x,y=y) ) + geom_path( data=ps, aes(x=x,y=y) )
print(p)
