# Sesión 4
# Postwork

# Utilizando la variable total_intl_charge de la base de datos
# telecom_service.csv de la sesión 3, realiza un análisis probabilístico. 

df <- read.csv("https://raw.githubusercontent.com/beduExpert/Programacion-R-Santander-2022/main/Sesion-03/Data/telecom_service.csv")
datos = df$total_intl_charge

# Para ello, debes determinar la función de distribución de probabilidad que más
# se acerque el comportamiento de los datos. Hint: Puedes apoyarte de medidas
# descriptivas o técnicas de visualización.

hist(datos)

# Suficientemente simétrica para usar aproximación a distribución normal.

# Una vez que hayas seleccionado el modelo, realiza lo siguiente:
#  1. Grafica la distribución teórica de la variable aleatoria total_intl_charge

media = mean(datos)
desv.est = sd(datos)
# x = seq(-4, 4, 0.01) * desv.est + media
curve(dnorm(x, mean = media, sd = desv.est), 
      from = media - 4 * desv.est,
      to = media + 4 * desv.est,
      xlab = "x", ylab = "f(x)")

# 2. ¿Cuál es la probabilidad de que el total de cargos internacionales sea
#   menor a 1.85 usd?

pnorm(q = 1.85, mean = media, sd = desv.est)
# 0.1125

# 3. ¿Cuál es la probabilidad de que el total de cargos internacionales sea
#    mayor a 3 usd?
pnorm(q = 3, mean = media, sd = desv.est, lower.tail = FALSE)
# 0.3774

# 4. ¿Cuál es la probabilidad de que el total de cargos internacionales esté
#    entre 2.35usd y 4.85 usd?
pnorm(q = 4.85, mean = media, sd = desv.est) - pnorm(q = 2.35, mean = media, sd = desv.est)
# 0.7060

# 5. Con una probabilidad de 0.48, ¿cuál es el total de cargos internacionales
#    más alto que podría esperar?
qnorm(p = 0.48, mean = media, sd = desv.est, lower.tail = FALSE)
# $2.80

# 6. ¿Cuáles son los valores del total de cargos internacionales que dejan
#    exactamente al centro el 80% de probabilidad?
c(qnorm(p = 0.1, mean = media, sd = desv.est), qnorm(p = 0.9, mean = media, sd = desv.est))
# $1.80 a $3.73