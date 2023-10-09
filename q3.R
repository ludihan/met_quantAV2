library(ggplot2)

dados = data.frame(
  Sp500 = c(1.2, -2.5, -3.0, 2.0, 5.0, 1.2, 3.0, -1.0, 0.5, 2.5),
  IBM = c(-0.7, -2.0, -5.5, 4.7, 1.8, 4.1, 2.6, 2.0, -1.3, 5.5)
)

#Passo 1:

grafico_dispersao = ggplot(data = dados, aes(x = Sp500, y = IBM)) +
  geom_point() +
  labs(x = "S&P 500", y = "IBM") +
  ggtitle("Gráfico de Dispersão: S&P 500 x IBM")

print(grafico_dispersao)

#Passo 2:

modelo_regressao = lm(IBM ~ Sp500, data = dados)
summary(modelo_regressao)

#Passo 3:

modelo_regressao = lm(IBM ~ Sp500, data = dados)
resumo = summary(modelo_regressao)
r_squared = resumo$r.squared
print(paste("Coeficiente de Determinação (R²):", round(r_squared, 4)))

#Passo 4:

correlacao = cor(dados$Sp500, dados$IBM)
print(paste("Coeficiente de Correlação (r):", round(correlacao, 4)))

#Passo 5:

media_sp500 = mean(dados$Sp500)
mediana_sp500 = median(dados$Sp500)
desvio_padrao_sp500 = sd(dados$Sp500)
variancia_sp500 = var(dados$Sp500)
minimo_sp500 = min(dados$Sp500)
maximo_sp500 = max(dados$Sp500)

media_ibm = mean(dados$IBM)
mediana_ibm = median(dados$IBM)
desvio_padrao_ibm = sd(dados$IBM)
variancia_ibm = var(dados$IBM)
minimo_ibm = min(dados$IBM)
maximo_ibm = max(dados$IBM)

print("Medidas para o S&P 500:")
print(paste("Média:", round(media_sp500, 4)))
print(paste("Mediana:", round(mediana_sp500, 4)))
print(paste("Desvio Padrão:", round(desvio_padrao_sp500, 4)))
print(paste("Variância:", round(variancia_sp500, 4)))
print(paste("Mínimo:", round(minimo_sp500, 4)))
print(paste("Máximo:", round(maximo_sp500, 4)))

print("Medidas para a IBM:")
print(paste("Média:", round(media_ibm, 4)))
print(paste("Mediana:", round(mediana_ibm, 4)))
print(paste("Desvio Padrão:", round(desvio_padrao_ibm, 4)))
print(paste("Variância:", round(variancia_ibm, 4)))
print(paste("Mínimo:", round(minimo_ibm, 4)))
print(paste("Máximo:", round(maximo_ibm, 4)))

#Passo 6:

boxplot_grafico = ggplot(data = dados, aes(y = IBM, x = Sp500)) +
  geom_boxplot() +
  labs(x = "S&P 500", y = "IBM") +
  ggtitle("Diagrama de Box Plot: S&P 500 e IBM")

print(boxplot_grafico)
