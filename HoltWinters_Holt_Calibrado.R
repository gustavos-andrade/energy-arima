dados_ex2 = read.xlsx("Exerc3_Dolar.xlsx", detectDates = T)

source("Funções.R")

dados_ex2 
View(dados_ex2)

# Dados de treino e teste

treino = dados_ex2[1:32,]
teste = dados_ex2[33:length(dados_ex2$Preço), ]


# Transformando em Time series - Demanda por mês

data_ts_treino = ts(data=treino$Preço, start = c(2018,5), end = c(2020,12),
                    frequency = 12)


data_ts_teste = ts(data=teste$Preço, start = c(2021,1), end = c(2021,6),
                   frequency = 12)



beta = seq(.01, 0.99, by = .01) # Define a sequência de valores treinados
acuracia = c()

#-----------------------------------------------------------------------------#

for(i in seq_along(beta)) {
  fit = holt(data_ts_treino,
             beta = beta[i])
  acuracia[i] = accuracy(fit, 
                         data_ts_teste)[2,5]
}

length(acuracia)

length(beta)
# Converte para dataframe e identifica melhor beta

beta.fit = cbind(beta, acuracia)
beta.fit<-as.data.frame(beta.fit)

beta.min = subset(beta.fit, 
                  acuracia == min(acuracia))

# Parâmetro alpha

alpha = seq(.01, 0.9, by = .01) # Define a sequência de valores treinados
acuracia = c()

for(i in seq_along(alpha)) {
  fit = holt(data_ts_treino,
             alpha = alpha[i])
  acuracia[i] = accuracy(fit, 
                         data_ts_teste)[2,5]
}

# Converte para dataframe e identifica melhor alpha

alpha.fit = cbind(alpha, acuracia)


alpha.min = subset(alpha.fit, 
                   acuracia == min(acuracia))


holt_model_calibrado = holt(data_ts_treino, alpha =alpha.min[1,1], beta = beta.min[1,1])
plot(holt_model_calibrado)


previsaoh_calibrado_H = predict(holt_model_calibrado, h = 6)

calc_mape(data_ts_teste, previsaoh_calibrado_H$mean)

#------------------------------------------------------------------------------#

beta = seq(.01, 0.99, by = .01) # Define a sequência de valores treinados
acuracia = c()

for(i in seq_along(beta)) {
  fit = HoltWinters(data_ts_treino,
             beta = beta[2])
  
  prev = predict(fit, n.ahead = 6)
  acuracia[i] = accuracy(prev, 
                         data_ts_teste)[1,5]
}

length(acuracia)

length(beta)
# Converte para dataframe e identifica melhor beta

beta.fit = cbind(beta, acuracia)
beta.fit<-as.data.frame(beta.fit)

beta.min = subset(beta.fit, 
                  acuracia == min(acuracia))

# Parâmetro alpha

alpha = seq(.01, 0.9, by = .01) # Define a sequência de valores treinados
acuracia = c()

for(i in seq_along(alpha)) {
  fit = HoltWinters(data_ts_treino,
                     beta = alpha[i])
  
  prev = predict(fit, n.ahead = 6)
  acuracia[i] = accuracy(prev, 
                         data_ts_teste)[1,5]
}

# Converte para dataframe e identifica melhor alpha

alpha.fit = cbind(alpha, acuracia)


alpha.min = subset(alpha.fit, 
                   acuracia == min(acuracia))



gamma = seq(.01, 0.9, by = .01) # Define a sequência de valores treinados
acuracia = c()

for(i in seq_along(alpha)) {
  fit = HoltWinters(data_ts_treino,
                    gamma =  gamma[i])
  
  prev = predict(fit, n.ahead = 6)
  acuracia[i] = accuracy(prev, 
                         data_ts_teste)[1,5]
}

# Converte para dataframe e identifica melhor alpha

gamma.fit = cbind(gamma, acuracia)


gamma.min = subset(gamma.fit, 
                   acuracia == min(acuracia))



holtWinter_model_calibrado = HoltWinters(data_ts_treino, alpha =alpha.min[1,1], beta = beta.min[1,1], gamma = gamma.min[1,1])
plot(holtWinter_model_calibrado)


previsaoh_calibrado = predict(holtWinter_model_calibrado, n.ahead =  6)

calc_mape(data_ts_teste, previsaoh_calibrado)
calc_mape(data_ts_teste, previsaoh_calibrado_H$mean)
