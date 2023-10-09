library(ggplot2)

dados = data.frame(
  Temperatura = c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190),
  Rendimento = c(45, 51, 54, 61, 66, 70, 74, 78, 85, 89)
)

#Passo 1:

grafico_dispersao = ggplot(dados, aes(x = Temperatura, y = Rendimento)) + 
  geom_point() + #pontos de dispersão
  labs(x = "Temperatura (°C)", y = "Rendimento (%)") +
  ggtitle("Diagrama de Dispersão de Temperatura vs. Rendimento")

print(grafico_dispersao)

#Passo 2:

modelo = lm(Rendimento ~ Temperatura, data = dados)
summary(modelo)

#Passo 3:
r_squared = summary(modelo)$r.squared
print(paste("Coeficiente de Determinação (R²):", round(r_squared, 4)))

#Passo 4:
temperatura_estimada = 155
rendimento_estimado = predict(modelo, newdata = data.frame(Temperatura = temperatura_estimada))
print(paste("Estimativa de Rendimento para", temperatura_estimada, "°C:", round(rendimento_estimado, 2)))