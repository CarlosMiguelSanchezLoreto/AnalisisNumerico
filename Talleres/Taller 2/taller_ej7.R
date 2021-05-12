###############################################################################
# EJERCICIO 7
###############################################################################
#Sea f(x) = e^x en el intervalo de [0, 1] utilice el metodo de lagrange y 
#determine el tamaño del paso que me produzca un error por debajo de 10−5.
#Utilizar el polinomio de Taylor para interpolar en este caso Verifique su respuesta
###############################################################################

##############################################################################
# SOLUCIÓN:
# MÉTODO DE LAGRANGE E INTERPOLACIÓN MEDIANTE POLINOMIOS DE TAYLOR
##############################################################################


require(pracma)

f <- function( x ) { exp(x) } 
plot(f,pch=19,cex=1,xlim=(0:1),col="blue")


#FUNCIÓN DE INTERPOLACIÓN DE LAGRANGE
lagrange = function(x,y,a){ 
  n = length(x) 
  if(a < min(x) || max(x) < a) stop("No está interpolando")
  X = matrix(rep(x, times=n), n, n, byrow=T)
  mN = a - X; diag(mN) = 1
  mD = X - t(X); diag(mD) = 1
  Lnk = apply(mN, 1, prod)/apply(mD, 2, prod)
  sum(y*Lnk)
}

x<-c(0,0.25,0.5,0.75,1)
y<-f(x)
r<-lagrange(x[1:5],y[1:5],0.25)
r
points(0.25,r)



#VERIFICACION POR POLINOMIO DE TAYLOR
f = function(x) 1/(1+25*x^2)
pol  = pracma::taylor(f, 0, 2) # FUNCION E^X, ALREDEDOR DE 0, GRADO 4.
printf(pol)

#GRAFICA DE LA FUNCIÓN ORIGINAL EN AZUL
curve(f, col= "blue", from = -1, to= 1)

#GRAFICA DEL POLINOMIO GENERADO EN ROJO
curve(pol[1]*x^4+pol[2]*x^3+pol[3]*x^2+pol[4]*x+pol[5],add=TRUE ,col="red", from = -1, to= 1)

#Transformar el polinomio en una función para que pueda ser evaluado
h<-function(x) pol[1]*x^4+pol[2]*x^3+pol[3]*x^2+pol[4]*x+pol[5]
h(1)

#Calcular el error
error<- f(1)-h(1)
cat("Error= ",error)


