#EJERCICIO 7.5
#DE UNA FUNCION F, CONOCEMOS LA SIGUIENTE INFORMACIÓN
#A)APROXIMAR USANDO LA REGLA DEL TRAPECIO
#B)APROXIMAR USANDO LA REGLA DE SIMPSON
#C)APROXIMAR USANDO ROMBERG

library(polynom)

h<-0.2
x<-c(0,0.2,0.4,0.6,0.8)
y<-c(3.592,3.110,3.017,2.865,2.658)

#GRAFICAR LA FUNCION DADOS LOS PUNTOS X,Y
plot(x,y,col="blue",type='l',lwd=2,main='FUNCION F(X)')

#DEFINIR FUNCION REGLA DEL TRAPECIO (TABLA DE VALORES DE F)
Trapecio2 <- function(y,h) {
  
  n = length(y)-1
  suma <- h * (abs(y[1]/2) + abs(sum(y[2:n])) + abs(y[n+1]/2))
  
  cat("Integral calculada: ",suma,"\t")
  errorE<- (((h*n)*h^2)/12)*1
  cat("Error estimado: ",errorE,"\n")
}


#DEFINIR FUNCION REGLA DE SIMPSON (TABLA DE VALORES DE F)
Simpson2 = function(y,h) {
  n = length(y)
  if (n%%2 != 1) stop("En la regla de Simpson, n es par!")
  i1<- seq(2, n-1, by = 2) # impares
  i2<- seq(3, n-2, by = 2) # pares)
  respuesta<-h/3* (y[1] + y[n] + 4*sum(y[i1]) + 2*sum(y[i2]))
  
  cat("Integral calculada: ",respuesta,"\t")
  errorE<- (((h*(n-1))*h^4)/180)*1
  cat("Error estimado: ",errorE,"\n")

}

#DEFINIR FUNCION ROMBERG
Romberg2<-function(f,a,b,n){
  
  respuesta<-pracma::romberg(f,a,b,n)
  cat("Integral calculada: ",respuesta$value,"\t")
  cat("Error estimado:",respuesta$rel.error,"\n")
  
}


#A)APROXIMAR USANDO LA REGLA DEL TRAPECIO
Trapecio2(y,h)


#B) APROXIMAR USANDO LA REGLA DE SIMPSON
Simpson2(y,h)


#C)APROXIMAR USANDO ROMBERG
pol <- poly.calc(x, y)
f = as.function(pol);
a<-0
b<-0.8
n<-10
Romberg2(f,a,b,n)

