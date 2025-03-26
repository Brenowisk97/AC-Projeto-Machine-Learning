# Carregar bibliotecas necessárias
library(caret)
library(plumber)
library(jsonlite)

# Carregar o dataset iris e preparar os dados para classificação binária
data("iris")
iris_binario <- subset(iris, Species %in% c("setosa", "versicolor"))
iris_binario$Species <- as.factor(ifelse(iris_binario$Species == "versicolor", 1, 0)) # Converte para binário

# Treinar um modelo de regressão logística
modelo_log <- train(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                    data = iris_binario, 
                    method = "glm", 
                    family = "binomial")

# Salvar o modelo treinado
saveRDS(modelo_log, "modelo_logistico_iris.rds")

#* @apiTitle API de Classificação de Espécies Iris
#* @param Sepal.Width Largura da Sépala
#* @param Petal.Length Comprimento da Pétala
#* @param Petal.Width Largura da Pétala
#* @get /prever
function(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) {
  Sepal.Length <- as.numeric(Sepal.Length)
  Sepal.Width <- as.numeric(Sepal.Width)
  Petal.Length <- as.numeric(Petal.Length)
  Petal.Width <- as.numeric(Petal.Width)
  
#* Realiza a previsão da espécie da flor com base nos parâmetros fornecidos
#* @param Sepal.Length Comprimento da Sépala

  # Verificar se algum parâmetro é inválido
  if (any(is.na(c(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))) {
    return(list(error = "Parâmetro inválido"))
  }
  
  # Criar um dataframe com os dados fornecidos
  dados <- data.frame(Sepal.Length = Sepal.Length,
                      Sepal.Width = Sepal.Width,
                      Petal.Length = Petal.Length,
                      Petal.Width = Petal.Width)
  
  # Fazer a previsão da probabilidade de ser "versicolor"
  prob <- predict(modelo_log, newdata = dados, type = "prob")[,2]
  classe_prevista <- ifelse(prob > 0.5, "versicolor", "setosa")
  
  return(list(
    Sepal.Length = Sepal.Length,
    Sepal.Width = Sepal.Width,
    Petal.Length = Petal.Length,
    Petal.Width = Petal.Width,
    probabilidade_versicolor = prob,
    classe_prevista = classe_prevista
  ))
}
