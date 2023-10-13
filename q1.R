library(ggplot2)

dados = data.frame(
  trafego = c(3,3,5,5,10,10,15,15,20,20,25,25,30,30),
  velocidade = c(95.6, 93.8, 74.4, 74.4, 50.5, 51.5, 44.6, 42.6, 35.8, 38.7, 32.0, 32.0, 30.1, 29.1)
)

#Passo 1:

grafico_dispersao = ggplot(dados, aes(x = trafego, y = velocidade)) +
  geom_point() + #pontos de dispersão
  labs(x = "Volume de Tráfego", y = "Velocidade Média") +
  ggtitle("Diagrama de Dispersão de Volume de Tráfego vs. Velocidade Média")

print(grafico_dispersao)

#Passo 2:

grafico_dispersao = ggplot(dados, aes(x = trafego, y = velocidade)) + 
  geom_smooth(method='lm', se=FALSE) +
  geom_point() + #pontos de dispersão
  labs(x = "Volume de Tráfego", y = "Velocidade Média") +
  ggtitle("Diagrama de Dispersão de Volume de Tráfego vs. Velocidade Média")

print(grafico_dispersao)

#Passo 3:

cor(dados$trafego,dados$velocidade)

#Passo 4: 
# Sim, as duas variáveis têm um coeficiente de correlação negativa forte de -0.90. 
# Vale notar que esse coeficiente sendo negativo indica que conforme uma variável aumenta a outra diminui, assim como o gráfico indica.