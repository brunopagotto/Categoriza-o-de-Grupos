### Dataframe apenas com os tempos de resposta

tempos_resposta = banco_dados_atualizado[-73]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(tempos_resposta)

# Crie um vetor de índices para as colunas pares
indices_colunas_pares <- seq(2, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
tempos_resposta <- tempos_resposta[, indices_colunas_pares]


tempos_resposta = tempos_resposta[c(12:36)]
#tempos_resposta$ID = banco_dados_atualizado$responseId

# Ordem desejada
colunas_ordem = colnames(tempos_resposta)

i = 1
linhas = 1
for (i in 1:nrow(tempos_resposta)) {
  
  # Transforme a única linha em um vetor
  vetor_valores <- unlist(tempos_resposta[i,])
  
  # Ordene o vetor em ordem crescente
  vetor_ordenado <- sort(vetor_valores)
  
  # Crie um novo dataframe com os valores ordenados em uma única linha
  dados_ordenados <- data.frame(t(vetor_ordenado))
  
  # Identificar linhas com o tempo de resposta do treino 02 errado
  if(dados_ordenados[[1]] < banco_dados_atualizado$treino02_rt[i]){
    indices_errados[linhas] <- i
    linhas = linhas + 1
  }
  
    # Substituir valor acumulado pelo valor exato do tempo de resposta
  for (j in 25:2) {
    dados_ordenados[[j]] = dados_ordenados[[j]] - dados_ordenados[[j - 1]]
  }
  dados_ordenados[[1]] = dados_ordenados[[1]] - banco_dados_atualizado$treino02_rt[i]
  
  # Ordene as colunas do dataframe 'dados_ordenados' na ordem desejada
  dados_ordenados <- dados_ordenados[, colunas_ordem]
  
  # Atribua os novos nomes às linhas do dataframe
  rownames(dados_ordenados) <- i
  
  # Encontre o índice da linha que você deseja substituir no dataframe 'tempos_resposta'
  linha_a_substituir <- i  # Substitua pelo índice da linha desejada
  
  # Selecione a linha correspondente no dataframe 'dados_ordenados'
  linha_dados_ordenados <- dados_ordenados[1, ]
  
  # Substitua a linha correspondente no dataframe 'tempos_resposta' pelos valores de 'dados_ordenados'
  tempos_resposta[linha_a_substituir, ] <- linha_dados_ordenados
}

tempos_resposta$ID = banco_dados_atualizado$responseId
tempos_resposta = tempos_resposta[c(26,1:25)]

write_xlsx(tempos_resposta, path = "Tempos Resposta Arrumados.xlsx")
write.csv(tempos_resposta, file = "Tempos Resposta Arrumados.csv", row.names = FALSE)

# Encontre os índices das células com valores negativos
indices_negativos <- which(tempos_resposta < 0, arr.ind = TRUE)

# Excluir linhas com o tempo de resposta do treino 02 errado
tempos_resposta = tempos_resposta[-indices_errados,]


## TESTE

  # Transforme a única linha em um vetor
  vetor_valores <- unlist(tempos_resposta[375,])
  
  # Ordene o vetor em ordem crescente
  vetor_ordenado <- sort(vetor_valores)
  
  # Crie um novo dataframe com os valores ordenados em uma única linha
  dados_ordenados <- data.frame(t(vetor_ordenado))
  
  # Substituir valor acumulado pelo valor exato do tempo de resposta
  for (j in 25:2) {
    dados_ordenados[[j]] = dados_ordenados[[j]] - dados_ordenados[[j - 1]]
  }
  dados_ordenados[[1]] = dados_ordenados[[1]] - banco_dados_atualizado$treino02_rt[i]
  
  # Ordene as colunas do dataframe 'dados_ordenados' na ordem desejada
  dados_ordenados <- dados_ordenados[, colunas_ordem]
  
  # Atribua os novos nomes às linhas do dataframe
  rownames(dados_ordenados) <- i
  
  # Encontre o índice da linha que você deseja substituir no dataframe 'tempos_resposta'
  linha_a_substituir <- i  # Substitua pelo índice da linha desejada
  
  # Selecione a linha correspondente no dataframe 'dados_ordenados'
  linha_dados_ordenados <- dados_ordenados[1, ]
  
  # Substitua a linha correspondente no dataframe 'tempos_resposta' pelos valores de 'dados_ordenados'
  tempos_resposta[linha_a_substituir, ] <- linha_dados_ordenados

  
### Após analisar os resultados, gráficos, etc., concluiu-se que era necessário
### um tratamento dos outliers, e o melhor resultado era estipulando limites
### inferior e superior a partir da media e 3 desvios padrões para cima e para
### baixo, e removendo os dados que não se adequassem. 866 dados.
  
dados_sem_outliers = tempos_resposta_backup

# Crie um loop para gerar os gráficos de distribuição
j = 1
questoes = 0
for (i in 2:26) {
  
  # Calcule os quartis
  media <- mean(dados_sem_outliers[[i]])
  desvio_padrao <- sd(dados_sem_outliers[[i]])
  
  # Defina um limite inferior e superior para os outliers
  limite_inferior <- media - 3 * desvio_padrao
  limite_superior <- media + 3 * desvio_padrao
  
  # Remova os outliers da coluna
  dados_sem_outliers <- dados_sem_outliers[dados_sem_outliers[[i]] >= limite_inferior & dados_sem_outliers[[i]] <= limite_superior, ]
  
  # Realize o teste de normalidade (Shapiro-Wilk)
  resultado_teste_normalidade <- shapiro.test(dados_sem_outliers[[i]])
  
  # Questões que apresentaram normalidade dos dados
  if(resultado_teste_normalidade$p.value > 0.05){
    questoes[j] = paste("teste", i-1)
    j=j+1
  }
  
  # Distribuição dos dados sem outliers usando ggplot2
  grafico_histograma <- ggplot(dados_sem_outliers, aes(x = dados_sem_outliers[[i]])) +
    geom_histogram(binwidth = 1) +  # Personalize o tamanho dos bins conforme necessário
    labs(title = paste("Distribuição de RT - Teste", i-1),
         x = "Valores",
         y = "Frequência")
  
  # Salve o gráfico como um arquivo PNG (ou outro formato desejado)
  nome_arquivo <- paste0("graficos_histograma/histograma_teste_", i-1, ".png")
  ggsave(nome_arquivo, plot = grafico_histograma, width = 6, height = 4)  # Ajuste o tamanho conforme necessário
  
}

write_xlsx(dados_sem_outliers, path = "Tempos Resposta Arrumados Sem Outliers.xlsx")
write.csv(dados_sem_outliers, file = "Tempos Resposta Arrumados Sem Outliers.csv", row.names = FALSE)

#tempos_resposta_backup = tempos_resposta
tempos_resposta = dados_sem_outliers

### Estatísticas para os testes com os tempos de resposta arrumados e sem outliers

# Crie vetores para armazenar os resultados
medias <- numeric(25)
variancias <- numeric(25)
desvios_padrao <- numeric(25)
minimos <- numeric(25)
maximos <- numeric(25)
soma <- numeric(25)
tamanho <- numeric(25)

# Use um loop para calcular as estatísticas para cada coluna
j = 1
for (i in 1:25) {
  coluna <- tempos_resposta[[j + 1]]  # Acesse a coluna pelo índice
  medias[i] <- mean(coluna)
  variancias[i] <- var(coluna)
  desvios_padrao[i] <- sd(coluna)
  minimos[i] <- min(coluna)
  maximos[i] <- max(coluna)
  soma[i] <- sum(coluna)
  tamanho[i] <- length(coluna)
  j = j + 1
}

# Exiba os resultados
resultados_testes <- data.frame(
  Teste = 1:25,
  Media = medias,
  Variancia = variancias,
  Desvio_Padrao = desvios_padrao,
  Minimo = minimos,
  Maximo = maximos,
  Soma = soma,
  Tamanho = tamanho
)

#resultados_testes[,c(2:7)] = round(resultados_testes[,c(2:7)], digits = 2)

# Encontre os índices das células com valores negativos
#indices_discrepantes <- which(tempos_resposta$teste20_rt > 9000, arr.ind = TRUE)

write_xlsx(resultados_atual, path = "Estatísticas Por Testes.xlsx")
write.csv(resultados_atual, file = "Estatísticas Por Testes.csv", row.names = FALSE)


### Estatísticas das 4 categorias

# Crie vetores para armazenar os resultados
medias <- numeric(4)
variancias <- numeric(4)
desvios_padrao <- numeric(4)
minimos <- numeric(4)
maximos <- numeric(4)
soma <- numeric(4)
tamanho <- numeric(4)

for (i in 1:4){
  
  if(i==1){
    dados_comb = tempos_resposta
    dados_comb$Coluna_Combinada <- paste(tempos_resposta$teste01_rt, tempos_resposta$teste02_rt, 
                                         tempos_resposta$teste03_rt, sep = ",")
  }
  
  if(i==2){
    dados_comb = tempos_resposta
    dados_comb$Coluna_Combinada <- paste(tempos_resposta$teste04_rt, tempos_resposta$teste05_rt, 
                                         tempos_resposta$teste06_rt, tempos_resposta$teste07_rt, 
                                         tempos_resposta$teste08_rt, sep = ",")
  }
  
  if(i==3){
    dados_comb = tempos_resposta
    dados_comb$Coluna_Combinada <- paste(tempos_resposta$teste09_rt, tempos_resposta$teste10_rt,
                                         tempos_resposta$teste11_rt, tempos_resposta$teste12_rt,
                                         tempos_resposta$teste13_rt, tempos_resposta$teste14_rt,
                                         tempos_resposta$teste15_rt, sep = ",")
  }
  
  if(i==4){
    dados_comb = tempos_resposta
    dados_comb$Coluna_Combinada <- paste(tempos_resposta$teste16_rt, tempos_resposta$teste17_rt, 
                                         tempos_resposta$teste18_rt, tempos_resposta$teste19_rt, 
                                         tempos_resposta$teste20_rt, tempos_resposta$teste21_rt,
                                         tempos_resposta$teste22_rt, tempos_resposta$teste23_rt, 
                                         tempos_resposta$teste24_rt, tempos_resposta$teste25_rt, 
                                         sep = ",")
  }
  
  dados_empilhados <- dados_comb %>%
    separate_rows(Coluna_Combinada, sep = ",")
  
  dados_empilhados$Coluna_Combinada = as.numeric(dados_empilhados$Coluna_Combinada)
  
  coluna <- dados_empilhados[[length(dados_empilhados)]]  # Acesse a coluna pelo índice
  medias[i] <- mean(coluna)
  variancias[i] <- var(coluna)
  desvios_padrao[i] <- sd(coluna)
  minimos[i] <- min(coluna)
  maximos[i] <- max(coluna)
  soma[i] <- sum(coluna)
  tamanho[i] <- length(coluna)
  
  # Exiba os resultados por categoria
  resultados_categoria <- data.frame(
    Categoria = 1:4,
    Media = medias,
    Variancia = variancias,
    Desvio_Padrao = desvios_padrao,
    Minimo = minimos,
    Maximo = maximos,
    Soma = soma,
    Tamanho = tamanho
  )
}

#resultados_categoria[,c(2:7)] = round(resultados_categoria[,c(2:7)], digits = 2)

write_xlsx(resultados_categoria, path = "Estatísticas por Categoria - rt.xlsx")
write.csv(resultados_categoria, file = "Estatísticas por Categoria - rt.csv", row.names = FALSE)

### até domingo
# Outliers
# Gráficos Distribuição
# Transformação
# Verificar Normalidade
# Fazer testes para os testes dentro das categorias
# Fazer testes para as categorias

#dados_sem_outliers = tempos_resposta

#which(dados_sem_outliers[,c(2:26)] < 1, arr.ind = TRUE)

# Crie um loop para gerar os gráficos de distribuição
#j = 1
#questoes = 0
for (i in 2:26) {
  
  # Calcule os quartis
  media <- mean(dados_sem_outliers[[i]])
  desvio_padrao <- sd(dados_sem_outliers[[i]])
  
  # Defina um limite inferior e superior para os outliers
  limite_inferior <- media - 3 * desvio_padrao
  limite_superior <- media + 3 * desvio_padrao
  
  # Remova os outliers da coluna
  dados_sem_outliers <- dados_sem_outliers[dados_sem_outliers[[i]] >= limite_inferior & dados_sem_outliers[[i]] <= limite_superior, ]
  
  # Realize o teste de normalidade (Shapiro-Wilk)
  resultado_teste_normalidade <- shapiro.test(dados_sem_outliers[[i]])
  
  # Questões que apresentaram normalidade dos dados
  if(resultado_teste_normalidade$p.value > 0.05){
    questoes[j] = paste("teste", i-1)
    j=j+1
  }
  
  # Distribuição dos dados sem outliers usando ggplot2
  grafico_histograma <- ggplot(dados_sem_outliers, aes(x = dados_sem_outliers[[i]])) +
    geom_histogram(binwidth = 1) +  # Personalize o tamanho dos bins conforme necessário
    labs(title = paste("Distribuição de RT - Teste", i-1),
         x = "Valores",
         y = "Frequência")
    
      # Salve o gráfico como um arquivo PNG (ou outro formato desejado)
  nome_arquivo <- paste0("graficos_histograma/histograma_teste_", i-1, ".png")
  ggsave(nome_arquivo, plot = grafico_histograma, width = 6, height = 4)  # Ajuste o tamanho conforme necessário
  
}

#write_xlsx(dados_sem_outliers, path = "Tempos Resposta Arrumados Sem Outliers.xlsx")
#write.csv(dados_sem_outliers, file = "Tempos Resposta Arrumados Sem Outliers.csv", row.names = FALSE)


### Transformação logarítmica dos dados para ter normalidade

log_tempos_resposta = tempos_resposta
log_tempos_resposta[,c(2:26)] = log(log_tempos_resposta[,c(2:26)])

#Colocar ID's no final
log_tempos_resposta = log_tempos_resposta[,c(2:26,1)]

# Crie vetores para armazenar os resultados
medias <- numeric(25)
variancias <- numeric(25)
desvios_padrao <- numeric(25)
minimos <- numeric(25)
maximos <- numeric(25)
soma <- numeric(25)
tamanho <- numeric(25)

j = 1
questoes = 0
for (i in 1:25) {
  
  coluna <- log_tempos_resposta[[i]]  # Acesse a coluna pelo índice
  medias[i] <- mean(coluna)
  variancias[i] <- var(coluna)
  desvios_padrao[i] <- sd(coluna)
  minimos[i] <- min(coluna)
  maximos[i] <- max(coluna)
  soma[i] <- sum(coluna)
  tamanho[i] <- nrow(log_tempos_resposta)
  
  # Exiba os resultados por categoria
  log_resultados_testes <- data.frame(
    Teste = 1:25,
    Media = medias,
    Variancia = variancias,
    Desvio_Padrao = desvios_padrao,
    Minimo = minimos,
    Maximo = maximos,
    Soma = soma,
    Tamanho = tamanho
  )
  
  # Distribuição dos dados sem outliers usando ggplot2
  grafico_histograma <- ggplot(log_tempos_resposta, aes(x = log_tempos_resposta[[i]])) +
    geom_histogram(binwidth = 0.1) +  # Personalize o tamanho dos bins conforme necessário
    labs(title = paste("Distribuição do log de RT - Teste", i),
         x = "Valores",
         y = "Frequência")
  
  # Salve o gráfico como um arquivo PNG (ou outro formato desejado)
  nome_arquivo <- paste0("graficos_histograma_transformados/histograma_teste_", i, ".png")
  ggsave(nome_arquivo, plot = grafico_histograma, width = 6, height = 4)  # Ajuste o tamanho conforme necessário
  
}

#log_resultados_testes[,c(2:7)] = round(log_resultados_testes[,c(2:7)], digits = 2)

write_xlsx(log_tempos_resposta, path = "Logaritmo dos Tempos Resposta Arrumados Sem Outliers.xlsx")
write.csv(log_tempos_resposta, file = "Logaritmo dos Tempos Resposta Arrumados Sem Outliers.csv", row.names = FALSE)

write_xlsx(log_resultados_testes, path = "Logaritmo das Estatísticas Por Testes Sem Outliers.xlsx")
write.csv(log_resultados_testes, file = "Logaritmo das Estatísticas Por Testes Sem Outliers.csv", row.names = FALSE)

# Crie vetores para armazenar os resultados
medias <- numeric(4)
variancias <- numeric(4)
desvios_padrao <- numeric(4)
minimos <- numeric(4)
maximos <- numeric(4)
soma <- numeric(4)
tamanho <- numeric(4)

for (i in 1:4){
  
  if(i==1){
    dados_comb = log_tempos_resposta
    dados_comb$Coluna_Combinada <- paste(log_tempos_resposta$teste01_rt, log_tempos_resposta$teste02_rt, 
                                         log_tempos_resposta$teste03_rt, sep = ",")
  }
  
  if(i==2){
    dados_comb = log_tempos_resposta
    dados_comb$Coluna_Combinada <- paste(log_tempos_resposta$teste04_rt, log_tempos_resposta$teste05_rt, 
                                         log_tempos_resposta$teste06_rt, log_tempos_resposta$teste07_rt, 
                                         log_tempos_resposta$teste08_rt, sep = ",")
  }
  
  if(i==3){
    dados_comb = log_tempos_resposta
    dados_comb$Coluna_Combinada <- paste(log_tempos_resposta$teste09_rt, log_tempos_resposta$teste10_rt,
                                         log_tempos_resposta$teste11_rt, log_tempos_resposta$teste12_rt,
                                         log_tempos_resposta$teste13_rt, log_tempos_resposta$teste14_rt,
                                         log_tempos_resposta$teste15_rt, sep = ",")
  }
  
  if(i==4){
    dados_comb = log_tempos_resposta
    dados_comb$Coluna_Combinada <- paste(log_tempos_resposta$teste16_rt, log_tempos_resposta$teste17_rt, 
                                         log_tempos_resposta$teste18_rt, log_tempos_resposta$teste19_rt, 
                                         log_tempos_resposta$teste20_rt, log_tempos_resposta$teste21_rt,
                                         log_tempos_resposta$teste22_rt, log_tempos_resposta$teste23_rt, 
                                         log_tempos_resposta$teste24_rt, log_tempos_resposta$teste25_rt, 
                                         sep = ",")
  }
  
  dados_empilhados <- dados_comb %>%
    separate_rows(Coluna_Combinada, sep = ",")
  
  dados_empilhados$Coluna_Combinada = as.numeric(dados_empilhados$Coluna_Combinada)
  
  coluna <- dados_empilhados[[length(dados_empilhados)]]  # Acesse a coluna pelo índice
  medias[i] <- mean(coluna)
  variancias[i] <- var(coluna)
  desvios_padrao[i] <- sd(coluna)
  minimos[i] <- min(coluna)
  maximos[i] <- max(coluna)
  soma[i] <- sum(coluna)
  tamanho[i] <- nrow(dados_empilhados)
  
  # Exiba os resultados por categoria
  log_resultados_categoria <- data.frame(
    Categoria = 1:4,
    Media = medias,
    Variancia = variancias,
    Desvio_Padrao = desvios_padrao,
    Minimo = minimos,
    Maximo = maximos,
    Soma = soma,
    Tamanho = tamanho
  )
  
  # Distribuição dos dados sem outliers usando ggplot2
  grafico_histograma_categorias <- ggplot(dados_empilhados, aes(x = Coluna_Combinada)) +
    geom_histogram(binwidth = 0.1) +  # Personalize o tamanho dos bins conforme necessário
    labs(title = paste("Distribuição do log de RT - Categoria", i),
         x = "Valores",
         y = "Frequência")
  
  # Salve o gráfico como um arquivo PNG (ou outro formato desejado)
  nome_arquivo <- paste0("graficos_histograma_categorias/histograma_categoria_", i, ".png")
  ggsave(nome_arquivo, plot = grafico_histograma_categorias, width = 6, height = 4)  # Ajuste o tamanho conforme necessário
    
}

write_xlsx(log_resultados_categoria, path = "Logaritmo das Estatísticas por Categoria sem Outliers - rt.xlsx")
write.csv(log_resultados_categoria, file = "Logaritmo das Estatísticas por Categoria sem Outliers - rt.csv", row.names = FALSE)


ani_fem_teste = 0
ani_mas_teste = 0
hum_fem_teste = 0
hum_mas_teste = 0
j = 1
k = 1
l = 1
m = 1
n = 1
for (i in 1:25) {
  if(i<3){
    for (j in (i+1):3) {
      # Realizar o teste de Mann-Whitney
      resultado_teste <- t.test(log_tempos_resposta[[i]], log_tempos_resposta[[j]])
      # Exibir os resultados do teste
      if(resultado_teste$p.value < 0.05){
        ani_fem_teste[k] = paste("teste",i,"X teste",j)
        k = k + 1
      }
    }
  }
  if(i>3 && i<8){
    for (j in (i+1):8) {
      # Realizar o teste de Mann-Whitney
      resultado_teste <- t.test(log_tempos_resposta[[i]], log_tempos_resposta[[j]])
      # Exibir os resultados do teste
      if(resultado_teste$p.value < 0.05){
        ani_mas_teste[l] = paste("teste",i,"X teste",j)
        l = l + 1
      }
    }
  }
  if(i>8 && i<15){
    for (j in (i+1):15) {
      # Realizar o teste de Mann-Whitney
      resultado_teste <- t.test(log_tempos_resposta[[i]], log_tempos_resposta[[j]])
      # Exibir os resultados do teste
      if(resultado_teste$p.value < 0.05){
        hum_fem_teste[m] = paste("teste",i,"X teste",j)
        m = m + 1
      }
    }
  }
  if(i>15 && i<25){
    for (j in (i+1):25) {
      # Realizar o teste de Mann-Whitney
      resultado_teste <- t.test(log_tempos_resposta[[i]], log_tempos_resposta[[j]])
      # Exibir os resultados do teste
      if(resultado_teste$p.value < 0.05){
        hum_mas_teste[n] = paste("teste",i,"X teste",j)
        n = n + 1
      }
    }
  }
}


for (i in 1:3) {
  
  for(j in (i+1):4){

    # Médias das duas amostras
    media_x <- resultados_categoria$Media[i]
    media_y <- resultados_categoria$Media[j]
    
    # Variâncias (ou desvios padrão) das duas amostras
    variancia_x <- resultados_categoria$Variancia[i] # Substitua com a variância ou desvio padrão da primeira amostra
    variancia_y <- resultados_categoria$Variancia[j] # Substitua com a variância ou desvio padrão da segunda amostra
    
    # Número de observações nas duas amostras
    n_x <- resultados_categoria$Tamanho[i] # Substitua com o número de observações da primeira amostra
    n_y <- resultados_categoria$Tamanho[j] # Substitua com o número de observações da segunda amostra
    
    # Nível de significância (alpha)
    alpha <- 0.05
    
    # Cálculo do valor t
    diferenca_medias <- media_x - media_y
    erro_padrao <- sqrt((variancia_x^2 / n_x) + (variancia_y^2 / n_y))
    graus_liberdade <- ((variancia_x^2 / n_x + variancia_y^2 / n_y)^2) / ((variancia_x^4 / (n_x^2 * (n_x - 1))) + (variancia_y^4 / (n_y^2 * (n_y - 1))))
    valor_t <- diferenca_medias / erro_padrao

    # Graus de liberdade para o teste t de Welch
    df_welch <- graus_liberdade
    
    # Valor crítico do teste t (bicaudal)
    valor_critico <- qt(1 - alpha / 2, df_welch)
    
    # Teste de significância
    if (abs(valor_t) > valor_critico) {
      resultado_teste <- "Rejeitar H0 (Diferença significativa)"
    } else {
      resultado_teste <- "Não rejeitar H0 (Sem diferença significativa)"
    }
    
    # Exibir resultados
    cat("Valor t:", valor_t, "\n")
    cat("Graus de liberdade (Welch):", df_welch, "\n")
    cat("Valor crítico do teste t:", valor_critico, "\n")
    cat("Resultado do teste entre a categoria", i, "e a categoria", j,":", resultado_teste, "\n")
  }
}


# Realizar o teste t de Welch
resultado_teste <- t.test(log_tempos_resposta$teste01_rt, log_tempos_resposta$teste02_rt, var.equal = FALSE)

# Exibir os resultados do teste
print(resultado_teste)




# Crie um QQ-plot dos dados transformados
qqnorm(dados_sem_outliers_2$teste01_rt)
qqline(dados_sem_outliers_2$teste01_rt)

which(dados_sem_outliers_2$teste25_rt < 1, arr.ind = TRUE)

mean(dados_sem_outliers_2$teste01_rt) - 2*sd(dados_sem_outliers_2$teste01_rt)


### DOMINGO: tentar log do log. Se ainda assim não funcionar, tratar como se
### fosse uma distribuição normal


#













### semana que vêm
# Testes de hipóteses para proporções nos testes dentro das categorias
# Testes de hipóteses para proporções entre as categorias
# Testes de hipóteses para proporções por variáveis sociodemográficas
