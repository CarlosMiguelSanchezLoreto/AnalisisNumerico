#EJERCICIO 7.4 

#LA FUNCION ERROR SE DEFINE COMO ERF(X) = 2/(sqrt(??))*e^(-t^(2)).
#APROXIMAR ERF(1.5) USANDO LOS METODOS DE INTEGRACION HASTA QUE LA DIFERENCIA EN CADA RESULTADO
#SEA <= 0.5*10^-5

require(pracma)

#DEFINIR LA FUNCION
f<- function(t) (2/sqrt(3.1416))*exp(-t^2)
a=0
b=1.5
tol=0.5*1e-5

#GRAFICAR LA FUNCION ORIGINAL
x<- seq(0,1.5,by=0.1)
y <- f(x)
plot(x,y,col="black",type='l',lwd=2)


#DEFINIR FUNCION REGLA DEL TRAPECIO
Trapecio<- function(f, a, b, n,tol){
  
  integral <- integrate(f,a,b)
  valor = integral$value
  errorE=1
  
  while(errorE >= tol){
  h=(b-a)/n
  xi<-a+(0:n)*h
  plot(f,col='black',type='l',lwd=5,main='REGLA DEL TRAPECIO')

    for(i in 1:n){
     segments(xi[i],0,xi[i+1],0,col='blue',lwd=2)
      segments(xi[i+1],0,xi[i+1],f(xi[i+1]),col='blue',lwd=2)
      segments(xi[i+1],f(xi[i+1]),xi[i],f(xi[i]),col='blue',lwd=2)
    }

    s<-(h/2)*(f(a)+f(b))
    suma<-(h/2)*(sum(2*f(a+(1:(n-1)*h))))
    
    respuesta<- s+suma
    errorE<- (((b-a)*h^2)/12)*1.1
    n<-n+1
  }
  cat("N: ",n,"\t")
  cat("Integral: ",respuesta,"\t")
  cat("Error estimado: ",errorE,"\n")
  
}


#DEFINIR FUNCION REGLA DEL SIMPSON
Simpson = function(f,a,b,n,tol) {
  
  integral <- integrate(f,a,b)
  valor = integral$value
  errorE=1
 
  if (n%%2 != 0) stop("En la regla de Simpson, n es par!")
  cat("Integral exacta: ",valor,"\n")
  
  while(errorE >= tol){
    h<- (b-a)/n
    i1<- seq(1, n-1, by = 2) # impares
    i2<- seq(2, n-2, by = 2) # pares
    y<- f(a+(0:n)*h) # f(a), f(a+h),...,f(a+i*h),...
    respuesta<- h/3 * ( f(a) + f(b) + 4*sum(y[i1]) + 2*sum(sum(y[i2]) ) )
  
    plot(y,col='blue',type='l',lwd=5,main='REGLA DEL SIMPSON')
  
    cat("N: ",n,"\t")
    cat("Integral calculada: ",respuesta,"\t")
    errorE<- (((b-a)*(h^4))/180)*1
    cat("Error estimado: ",errorE,"\n")
    n<-n+1
  } 
}

#DEFINIR FUNCION ROMBERG
Romberg<-function(f,a,b,n,tol){
  
  respuesta<-pracma::romberg(f,a,b,n,tol)
  
  cat("N: ",respuesta$iter,"\t")
  cat("Integral calculada: ",respuesta$value,"\t")
  cat("Error estimado:",respuesta$rel.error,"\n")
  
}

#DEFINIR FUNCION CUADRATURA GAUSSIANA
QuadGauss<-function(f,a,b,n){
  respuesta<- pracma::quadl(f,a,b,tol)
  cat("Integral calculada: ",respuesta,"\t")
}

#APROXIMAR Erf(1.5) REGLA DEL TRAPECIO
n=1
Trapecio(f,a,b,n,tol)

#APROXIMAR Erf(1.5) REGLA DE SIMPSON
n=4
Simpson(f,a,b,n,tol)

#APROXIMAR Erf(1.5) ROMBERG
n=5
Romberg(f,a,b,n,tol)

#APROXIMAR Erf(1.5) CUADRATURA GAUSSIANA
QuadGauss(f,a,b,n)


