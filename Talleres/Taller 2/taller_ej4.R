###############################################################################
# EJERCICIO 4
###############################################################################
# Con la función f(x) = lnx construya la interpolación de diferencias divididas 
# en x0 = 1; x1 =2, que le permita incluir la información de f(1); f'(1); f(2); f'(2) 
# trabajando con dos digitos decimales y estime el error en [1, 2]
###############################################################################

##############################################################################
# SOLUCIÓN:
# Método Hermite mediante Diferencias Divididas
# 
##############################################################################

Fx <- function(x) { return(1/(1+25*x^2)) }

x <- c(-1, 1)	# Puntos x0 = 1, x1 = 2
y <- c(0.03, 0.03)	
dx <- c(0.07, -0.07)	

# Interpolación manual por Hermite - Diferencias divididas
z0 <- x[1]
z1 <- x[1]
z2 <- x[2]
z3 <- x[2]

fz0 <- y[1]
fz1 <- y[1]
fz2 <- y[2]
fz3 <- y[2]

d01 <- dx[1]
d12 <- (fz2 - fz1) / (z2 - z1)
d23 <- dx[2]

d012 <- (d12 -d01) / (z2 - z0)
d123 <- (d23 - d12) / (z3 - z1)
d0123 <- (d123 - d012) / (z3 - z0)

# Polinomio generado
Px <- function(x) { fz0*x + d01*(x - z0) + d012*(x - z0)*(x - z1) + d0123*(x - z0)*(x - z1)*(x - z2) }

# Polinomio error (f(x) - P(x))
Ex <- function(x) { 1/(1+25*x^2) - (fz0 + d01*(x - z0) + d012*(x - z0)*(x - z1) + d0123*(x - z0)*(x - z1)*(x - z2)) }

# Errores
Ex(-1)	# Error en el punto x0 = 1
Ex(1)	# Error en el punto x1 = 2

plot.new()
plot(Px)
curve(Fx, from=-1, to=1, col="blue", main="Gráfica del ejercicio 4")
curve(Px, from=-1, to=1, col="red", add=TRUE)
curve(Ex, from=-1, to=1, col="green", add=TRUE)
legend(1.6, 0.4, c("f(x) = ln(x)", "P(x)", "E(x)"), col=c("blue","red","green"))