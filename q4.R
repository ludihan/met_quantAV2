#Passo 1:

equacao_regressao = function(X) {
  a = (20 * 1083.67 - 1478 * 12.75) / 
    (20 * 143215.8 - (1478 ** 2))
  b = (12.75 / 20) - a * (1478 / 20)
  # (0.004161175 * X + 0.3299892)
  return(a * X + b)
}

#Passo 2:
equacao_regressao(85)

#Passo 3:
equacao_regressao(85) - equacao_regressao(85 + 1)