## SERIE TEMPORAL DENGUE NO MUNICIPIO DE VITORIA ##

##### 2013 - 2024 #####
##### PACOTES #####
install.packages("pacman")
library(pacman)
pacman::p_load(tidyverse, dplyr, lubridate, ggplot2, ggTimeSeries, tseries,
               forecast, prophet, lubridate, Metrics)

##### IMPORTANDO DADOS INFODENGUE #####
# tutorial deles
url <- "https://info.dengue.mat.br/api/alertcity?"
geocode <- 3205309
disease <- "dengue"
format <- "csv"
ew_start <- 1
ew_end <- 52
ey_start <- 2014
ey_end <- 2024


cons1 <- paste0(url,"geocode=",geocode,"&disease=",disease,"&format=",format,
                "&ew_start=",ew_start,"&ew_end=",ew_end,"&ey_start=",ey_start,
                "&ey_end=",ey_end)
cons1

dados <- read_csv(cons1, show_col_types=FALSE) %>% arrange(data_iniSE)
glimpse(dados)

##### ORGANIZANDO DF #####
semanaXcasos <- dados[,1:6] # deixando na base só casos por semana

mesXcasos <- semanaXcasos %>%
  mutate(mes = floor_date(as.Date(data_iniSE), "month")) %>%
  group_by(mes) %>%
  summarise(
    casos = sum(casos, na.rm = TRUE),
    casos_est = sum(casos_est, na.rm = TRUE),
    casos_est_min = sum(casos_est_min, na.rm = TRUE),
    casos_est_max = sum(casos_est_max, na.rm = TRUE)
  )

# agora tenho casos por mes de 2014 a 2024

mesXcasos[133,1] - mesXcasos[1,1] #aproximadamente 11 anos

mesXcasos <- mesXcasos[,-3:-5]

##### GRÁFICOS INICIAIS #####

ggplot(mesXcasos, aes(x = mes, y = casos)) +
  geom_line(color = "red") +
  labs(title = "Casos mensais de dengue",
       x = "Data", y = "Casos") +
  theme_minimal()

#Weekly count of reported cases of dengue in the city of Rio de Janeiro. Arrows indicate weekly variation.
p1 <- ggplot_waterfall(mesXcasos,'mes','casos', nArrowSize = 0.8)
p1 + scale_fill_manual(
  values = c("forestgreen", "blue", "darkred"),
  labels = c("4wd", "front", " rear")) +
  xlab("Epidemiological Week")+
  ylab("Reported Cases")+
  scale_x_continuous(breaks=mesXcasos$casos)+
  theme_light() +
  theme(legend.position="none",axis.text.x = element_text(angle = 45,size=23, hjust = 1), 
        axis.text.y = element_text(size=23),axis.title= element_text(size=30))


##### SÉRIE TEMPORAL #####
serie_dengue <- ts(semanaXcasos$casos, frequency = 52, start = c(2013,52))
serie_dengue

#plotando serie
plot.ts(serie_dengue) # 2021 é um ano de zeros, alguma coisa aí




##### 2015 - 2024 #####
###### IMPORTANDO NOVOS DADOS #####
# tutorial deles
url1 <- "https://info.dengue.mat.br/api/alertcity?"
geocode1 <- 3205309
disease1 <- "dengue"
format1 <- "csv"
ew_start1 <- 1
ew_end1 <- 52
ey_start1 <- 2015
ey_end1 <- 2020


cons2 <- paste0(url1,"geocode=",geocode1,"&disease=",disease1,"&format=",format1,
                "&ew_start=",ew_start1,"&ew_end=",ew_end1,"&ey_start=",ey_start1,
                "&ey_end=",ey_end1)
cons2

dados1 <- read_csv(cons2, show_col_types=FALSE) %>% arrange(data_iniSE)
glimpse(dados1)


##### ORGANIZANDO DF #####
semanaXcasos1 <- dados1[,c("data_iniSE", "SE", "casos")]

##### TRANSFORMANDO EM SERIE #####
serie_dengue1 <- ts(semanaXcasos1$casos, frequency = 52, start = c(2015,1))
serie_dengue1

plot.ts(serie_dengue1)
##### GRÁFICOS INICIAIS #####
#plotando serie
plot.ts(serie_dengue1)

# e se eu fizer uma serie de cada ano?
intervalo15 <- semanaXcasos1[1:52,]
intervalo16 <- semanaXcasos1[53:104,]
intervalo17 <- semanaXcasos1[105:156,]
intervalo18 <- semanaXcasos1[157:208,]
intervalo19 <- semanaXcasos1[209:260,]
intervalo20 <- semanaXcasos1[261:312,]


#gráfico janela 2015
plot_dengue <- function(df, titulo) {
  
  df$SE <- as.numeric(substr(df$SE, 5, 6))  # Extrai as semanas como valores numéricos
  
  ggplot(df, aes(x = SE, y = casos)) +
    geom_line(color = "blue", size = 1) +  # Linha azul, espessura 1
    labs(title = titulo,
         x = "Semana Epidemiológica",
         y = "Número de Casos") +
    theme_minimal() +  # Tema simples e limpo
    theme(
      plot.title = element_text(hjust = 0.5, size = 14),  # Centraliza o título
      axis.title = element_text(size = 12),  # Tamanho do título dos eixos
      axis.text = element_text(size = 10, angle = 45, hjust = 1)  # Tamanho e rotação dos textos
    ) +
    scale_x_continuous(
      breaks = 1:52,  # Quebra para semanas de 1 a 52
      labels = 1:52   # Exibe as semanas numeradas de 1 a 52
    )
}

#lista de df
lista_intervalos <- list(intervalo15, intervalo16, intervalo17, intervalo18, 
                         intervalo19, intervalo20)
nomes_intervalos <- c("2015", "2016", "2017", "2018", "2019", "2020")

# Loop para plotar todos os gráficos
for (i in 1:length(lista_intervalos)) {
  print(plot_dengue(lista_intervalos[[i]], paste("Casos de Dengue em", nomes_intervalos[i])))
}


##### DECOMPOSIÇÃO DA SÉRIE #####
decomposicao1 <- decompose(serie_dengue1)

plot(decomposicao1)

##### VERFICAÇÃO RESIDUOS/RUÍDO #####
acf(decomposicao1$random, na.action = na.pass, main = "ACF dos Resíduos")

##### VERIFICAR SERIE ESTACIONARIA #####
adf.test(serie_dengue1)


##### SEPARANDO TESTE E TREINO E VALIDAÇAO #####
# Separando os dados
treino <- window(serie_dengue1, end = c(2018, 52))
validacao <- window(serie_dengue1, start = c(2019, 1), end = c(2019, 52))
teste <- window(serie_dengue1, start = c(2020, 1), end = c(2020, 52))

##### SARIMA #####

modelo_sarima <- auto.arima(treino, seasonal = TRUE, 
                            stepwise = FALSE,
                            approximation = FALSE)
summary(modelo_sarima)

# Rolling Forecast  (validação)
preds_sarima <- numeric(length(validacao))
dados <- treino
for (i in 1:length(validacao)) {
  mod <- auto.arima(dados, seasonal = TRUE)
  preds_sarima[i] <- forecast(mod, h = 1)$mean[1]
  dados <- ts(c(dados, validacao[i]), frequency = 52, start = c(2015, 1))
}


##### HOLT WINTERS #####
#nao funcionou muito bem
# Ajuste do modelo Holt-Winters com sazonalidade multiplicativa
modelo_hw <- hw(treino, seasonal = "multiplicative")

# Verificando o resumo do modelo
summary(modelo_hw)

# Realizando previsões de 1 passo à frente
previsao_hw <- forecast(modelo_hw, h = 1)

# Plotando a previsão
autoplot(previsao_hw)

##### PROPHET #####

dados_prophet <- data.frame(ds = semanaXcasos1$data_iniSE, y = semanaXcasos1$casos)
print(head(dados_prophet)) 

# Separando treino e validação (2019)
treino_prophet <- dados_prophet[dados_prophet$ds < "2019-01-01", ]
validacao_prophet <- dados_prophet[dados_prophet$ds >= "2019-01-01", ]

# Ajustando o modelo Prophet com os dados de treino
modelo_prophet <- prophet(treino_prophet)

# Realizando previsões para 1 passo à frente
futuro <- make_future_dataframe(modelo_prophet, periods = 1, freq = "week")
previsao_prophet <- predict(modelo_prophet, futuro)

# Visualizando a previsão
plot(modelo_prophet, previsao_prophet)

# Rolling Forecast 1 passo
preds_hw <- numeric(length(validacao))
dados <- treino
for (i in 1:length(validacao)) {
  mod <- hw(dados, seasonal = "additive")
  preds_hw[i] <- forecast(mod, h = 1)$mean[1]
  dados <- ts(c(dados, validacao[i]), frequency = 52, start = c(2015, 1))
}
