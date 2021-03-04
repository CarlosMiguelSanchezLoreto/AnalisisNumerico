# Remueve todos los objetos creados
rm(list=ls())


obtenerPrecision <- function(tol) {
  n = log(tol, 10) * -1
  return (n)
}

fmt <- function(n, precision) {
  return (formatC(n, format = "e", digits=precision))
}

DD <- function(expr, name, order = 1) {
  if(order < 1) stop("'order' must be >= 1")
  if(order == 1) D(expr, name)
  else DD(D(expr, name), name, order - 1)
}


Taylor = function(f, x0, a, n){ 
  
  precision <- obtenerPrecision(1.e-9)
  
  #convertir f en una expresion
  g = parse(text=f)
  
  # convertir g en función
  Fx = function(x){eval(g[[1]])}

  
  p = rep(NA, length=n+1)
  
  for(i in 1:n){
    D = DD(g,"x", i)
    Dx = function(x) eval(D)
    p[i]=1/factorial(i)*(x0-a)^i *Dx(a)
    
    error <- abs(Fx(x0)-p[i])
    cat("\t i=",i,"\t P=",fmt(p[i],precision),"\tE= ",fmt(error,precision),"\n")
  }
  p[n+1] = Fx(a)
  sum(p)
  

   x <- seq(-0.5, 0.5, length.out=100)
   yf <- Fx(x)
   plot(x, yf, type = "l", col = "gray", lwd = 3)
   lines(x, col = "red")
   grid()
  
  
}
Taylor("log(1+x)", 0.005, 0, 3)
Taylor("log(1+x)", 0.0001, 0, 3)
Taylor("log(1+x)", 0.499999999, 0, 3)





