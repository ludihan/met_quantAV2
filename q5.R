library(ggplot2)

dados = data.frame(
  Idade = c(28, 32, 45, 29, 38, 50, 42, 35, 31, 48, 27, 39, 33, 47, 36, 30, 44, 37, 34, 41),
  Salario = c(3500, 4200, 5500, 3600, 4800, 6000, 5200, 4500, 4100, 5800, 3400, 4900, 4300, 5700, 4600, 4000, 5400, 4700, 4400, 5100)
)

#Passo 1:

grafico_dispersao = ggplot(data = dados, aes(x = Idade, y = Salario)) +
  geom_point() +
  labs(x = "Idade", y = "Salário")
  ggtitle("Gráfico de Dispersão: Idade x Salário")
  
print(grafico_dispersao)

#Passo 2:

modelo_regressao = lm(Salario ~ Idade, data = dados)
coeficientes = coef(modelo_regressao)
cat("Equação de regressão linear: Salário = ", coeficientes[1], " + ", coeficientes[2], " * Idade\n")

#passo 3:

resumo_modelo = summary(modelo_regressao)
r_quadrado = resumo_modelo$r.squared
cat("Coeficiente de Determinação (R²): ", r_quadrado, "\n")

#Passo 4:

coeficiente_Correlacao = cor(dados$Idade, dados$Salario)
cat("Coeficiente de Correlação (r): ", coeficiente_Correlacao, "\n")

#Passo 5:

media_idade = mean(dados$Idade)
mediana_idade = median(dados$Idade)

desvio_padrao_idade = sd(dados$Idade)
iqr_idade = IQR(dados$Idade)

media_salario = mean(dados$Salario)
mediana_salario = median(dados$Salario)


desvio_padrao_salario = sd(dados$Salario)
iqr_salario = IQR(dados$Salario)

cat("Medidas de Posição e Dispersão para Idade:\n")
cat("Média da Idade: ", media_idade, "\n")
cat("Mediana da Idade: ", mediana_idade, "\n")
cat("Desvio Padrão da Idade: ", desvio_padrao_idade, "\n")
cat("IQR (Intervalo Interquartil) da Idade: ", iqr_idade, "\n\n")

cat("Medidas de Posição e Dispersão para Salário:\n")
cat("Média do Salário: ", media_salario, "\n")
cat("Mediana do Salário: ", mediana_salario, "\n")
cat("Desvio Padrão do Salário: ", desvio_padrao_salario, "\n")
cat("IQR (Intervalo Interquartil) do Salário: ", iqr_salario, "\n")

#Passo 6:

boxplot_idade = ggplot(data = dados, aes(y = Idade)) +
  geom_boxplot() +
  labs(y = "Idade", title = "Box Plot: Idade")

boxplot_salario = ggplot(data = dados, aes(y = Salario)) +
  geom_boxplot() +
  labs(y = "Salário", title = "Box Plot: Salário")

print(boxplot_idade)
print(boxplot_salario)