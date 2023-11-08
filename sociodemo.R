
### Análise das variáveis sociodemográficas ###

sociodemo = banco_dados_atualizado

sociodemo <- as.data.frame(lapply(sociodemo, function(x) gsub("neut.ani", "Machos e Fêmeas", x)))
sociodemo <- as.data.frame(lapply(sociodemo, function(x) gsub("neut.hum", "Homens e Mulheres", x)))
sociodemo <- as.data.frame(lapply(sociodemo, function(x) gsub("exc.ani.macho", "Apenas Machos", x)))
sociodemo <- as.data.frame(lapply(sociodemo, function(x) gsub("exc.ani.femea", "Apenas Fêmeas", x)))
sociodemo <- as.data.frame(lapply(sociodemo, function(x) gsub("exc.hum.homem", "Apenas Homens", x)))
sociodemo <- as.data.frame(lapply(sociodemo, function(x) gsub("exc.hum.mul", "Apenas Mulheres", x)))

## Gênero

sociodemo$socioidgenero = gsub("\\[|\\]", "", sociodemo$socioidgenero)
sociodemo$socioidgenero = gsub("\"", "", sociodemo$socioidgenero)
sociodemo$socioidgenero = gsub("hom.cis", "Homem (Cis)", sociodemo$socioidgenero)
sociodemo$socioidgenero = gsub("mul.cis", "Mulher (Cis)", sociodemo$socioidgenero)

mulheres_cis = subset(sociodemo, socioidgenero == "Mulher (Cis)")
homens_cis = subset(sociodemo, socioidgenero == "Homem (Cis)")

# Mulheres
dados_comb = mulheres_cis
dados_comb$Coluna_Combinada <- paste(mulheres_cis$teste01, mulheres_cis$teste02, mulheres_cis$teste03, sep = ",")

dados_comb = mulheres_cis
dados_comb$Coluna_Combinada <- paste(mulheres_cis$teste04, mulheres_cis$teste05, mulheres_cis$teste06,
                                     mulheres_cis$teste07, mulheres_cis$teste08, sep = ",")

dados_comb = mulheres_cis
dados_comb$Coluna_Combinada <- paste(mulheres_cis$teste09, mulheres_cis$teste10, mulheres_cis$teste11,
                                     mulheres_cis$teste12, mulheres_cis$teste13, mulheres_cis$teste14,
                                     mulheres_cis$teste15, sep = ",")

dados_comb = mulheres_cis
dados_comb$Coluna_Combinada <- paste(mulheres_cis$teste16, mulheres_cis$teste17, mulheres_cis$teste18,
                                     mulheres_cis$teste19, mulheres_cis$teste20, mulheres_cis$teste21,
                                     mulheres_cis$teste22, mulheres_cis$teste23, mulheres_cis$teste24,
                                     mulheres_cis$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Respondentes Mulheres Cis - Testes 16 a 25",
       x = "Respostas",
       y = "Contagem") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink",
                               "Apenas Machos" = "blue",
                               "Machos e Fêmeas" = "purple",
                               "Apenas Mulheres" = "red",
                               "Apenas Homens" = "green",
                               "Homens e Mulheres" = "yellow")) +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta a legenda de cores

# Exiba o gráfico de histograma
print(grafico_histograma_comb)

# Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
ggsave(filename = paste("Mulheres/Categoria 4.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)


# Homens
dados_comb = homens_cis
dados_comb$Coluna_Combinada <- paste(homens_cis$teste01, homens_cis$teste02, homens_cis$teste03, sep = ",")

dados_comb = homens_cis
dados_comb$Coluna_Combinada <- paste(homens_cis$teste04, homens_cis$teste05, homens_cis$teste06,
                                     homens_cis$teste07, homens_cis$teste08, sep = ",")

dados_comb = homens_cis
dados_comb$Coluna_Combinada <- paste(homens_cis$teste09, homens_cis$teste10, homens_cis$teste11,
                                     homens_cis$teste12, homens_cis$teste13, homens_cis$teste14,
                                     homens_cis$teste15, sep = ",")

dados_comb = homens_cis
dados_comb$Coluna_Combinada <- paste(homens_cis$teste16, homens_cis$teste17, homens_cis$teste18,
                                     homens_cis$teste19, homens_cis$teste20, homens_cis$teste21,
                                     homens_cis$teste22, homens_cis$teste23, homens_cis$teste24,
                                     homens_cis$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Respondentes Homens Cis - Testes 1 a 3",
       x = "Respostas",
       y = "Contagem") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink",
                               "Apenas Machos" = "blue",
                               "Machos e Fêmeas" = "purple",
                               "Apenas Mulheres" = "red",
                               "Apenas Homens" = "green",
                               "Homens e Mulheres" = "yellow")) +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta a legenda de cores

# Exiba o gráfico de histograma
print(grafico_histograma_comb)

# Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
ggsave(filename = paste("Homens/Categoria 1.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)


# Mulheres

# Selecionando apenas as colunas do banco de dados referente aos testes
mulheres_cis = mulheres_cis[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(mulheres_cis)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
mulheres_cis <- mulheres_cis[, indices_colunas_impares]

# Renomeando os valores para deixar as colunas binárias
mulheres_cis[,c(1:3)] <- as.data.frame(lapply(mulheres_cis[,c(1:3)], 
                                            function(x) ifelse( x!= "Apenas Fêmeas", 
                                                                "Outra Resposta", x)))

mulheres_cis[,c(4:8)] <- as.data.frame(lapply(mulheres_cis[,c(4:8)], 
                                            function(x) ifelse( x!= "Machos e Fêmeas", 
                                                                "Outra Resposta", x)))

mulheres_cis[,c(9:15)] <- as.data.frame(lapply(mulheres_cis[,c(9:15)], 
                                             function(x) ifelse( x!= "Apenas Mulheres", 
                                                                 "Outra Resposta", x)))

mulheres_cis[,c(16:25)] <- as.data.frame(lapply(mulheres_cis[,c(16:25)], 
                                              function(x) ifelse( x!= "Homens e Mulheres", 
                                                                  "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
mulheres_categoria_um_p = sum(mulheres_cis[c(1:3)] == "Apenas Fêmeas")/(3*nrow(mulheres_cis))
mulheres_categoria_dois_p = sum(mulheres_cis[c(4:8)] == "Machos e Fêmeas")/(5*nrow(mulheres_cis))
mulheres_categoria_tres_p = sum(mulheres_cis[c(9:15)] == "Apenas Mulheres")/(7*nrow(mulheres_cis))
mulheres_categoria_quatro_p = sum(mulheres_cis[c(16:25)] == "Homens e Mulheres")/(10*nrow(mulheres_cis))

categorias = c(mulheres_categoria_um_p, mulheres_categoria_dois_p,
               mulheres_categoria_tres_p, mulheres_categoria_quatro_p)

mulheres_categorias = categorias

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(mulheres_cis), 4))

mulheres_tamanho_categoria = tamanho_categoria

for (i in 1:3) {
  for (j in (i+1):4){
    categoria_i_p = categorias[i]
    categoria_i_1_p = 1 - categorias[i]
    categoria_j_p = categorias[j]
    categoria_j_1_p = 1 - categorias[j]
    
    # Estatística do Teste
    Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / tamanho_categoria[j]))
    
    # Testes comparados
    print(paste("Categoria", i, "X Categoria", j))
    
    # Proporções
    print(paste("Proporção da Categoria", i, "=", round(categoria_i_p, digits = 3)))
    print(paste("Proporção da Categoria", j, "=", round(categoria_j_p, digits = 3)))
    
    # Tamanho das amostras
    #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
    #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
    
    # Teste de significância
    if (abs(Z) > regiao_critica) {
      print(paste("Rejeitar H0 ( Diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    } else {
      print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    }
  }
}

# Homens

# Selecionando apenas as colunas do banco de dados referente aos testes
homens_cis = homens_cis[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(homens_cis)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
homens_cis <- homens_cis[, indices_colunas_impares]

# Renomeando os valores para deixar as colunas binárias
homens_cis[,c(1:3)] <- as.data.frame(lapply(homens_cis[,c(1:3)], 
                                              function(x) ifelse( x!= "Apenas Fêmeas", 
                                                                  "Outra Resposta", x)))

homens_cis[,c(4:8)] <- as.data.frame(lapply(homens_cis[,c(4:8)], 
                                              function(x) ifelse( x!= "Machos e Fêmeas", 
                                                                  "Outra Resposta", x)))

homens_cis[,c(9:15)] <- as.data.frame(lapply(homens_cis[,c(9:15)], 
                                               function(x) ifelse( x!= "Apenas Mulheres", 
                                                                   "Outra Resposta", x)))

homens_cis[,c(16:25)] <- as.data.frame(lapply(homens_cis[,c(16:25)], 
                                                function(x) ifelse( x!= "Homens e Mulheres", 
                                                                    "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
homens_categoria_um_p = sum(homens_cis[c(1:3)] == "Apenas Fêmeas")/(3*nrow(homens_cis))
homens_categoria_dois_p = sum(homens_cis[c(4:8)] == "Machos e Fêmeas")/(5*nrow(homens_cis))
homens_categoria_tres_p = sum(homens_cis[c(9:15)] == "Apenas Mulheres")/(7*nrow(homens_cis))
homens_categoria_quatro_p = sum(homens_cis[c(16:25)] == "Homens e Mulheres")/(10*nrow(homens_cis))

categorias = c(homens_categoria_um_p, homens_categoria_dois_p,
                      homens_categoria_tres_p, homens_categoria_quatro_p)

homens_categorias = categorias

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(homens_cis), 4))

homens_tamanho_categoria = tamanho_categoria

for (i in 1:3) {
  for (j in (i+1):4){
    categoria_i_p = categorias[i]
    categoria_i_1_p = 1 - categorias[i]
    categoria_j_p = categorias[j]
    categoria_j_1_p = 1 - categorias[j]
    
    # Estatística do Teste
    Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / tamanho_categoria[j]))
    
    # Testes comparados
    print(paste("Categoria", i, "X Categoria", j))
    
    # Proporções
    print(paste("Proporção da Categoria", i, "=", round(categoria_i_p, digits = 3)))
    print(paste("Proporção da Categoria", j, "=", round(categoria_j_p, digits = 3)))
    
    # Tamanho das amostras
    #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
    #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
    
    # Teste de significância
    if (abs(Z) > regiao_critica) {
      print(paste("Rejeitar H0 ( Diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    } else {
      print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    }
  }
}


# Testes de Hipóteses comparando as proporções mais prevalentes entre homens e 
# mulheres de cada uma das 4 categorias

for (i in 1:4) {

  categoria_i_p = mulheres_categorias[i]
  categoria_i_1_p = 1 - mulheres_categorias[i]
  categoria_j_p = homens_categorias[i]
  categoria_j_1_p = 1 - homens_categorias[i]
  
  # Estatística do Teste
  Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / mulheres_tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / homens_tamanho_categoria[i]))
  
  # Testes comparados
  print(paste("Categoria", i))
  
  # Proporções
  print(paste("Proporção das Mulheres Cis =", round(categoria_i_p, digits = 3)))
  print(paste("Proporção dos Homens Cis =", round(categoria_j_p, digits = 3)))
  
  # Tamanho das amostras
  #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
  #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
  
  # Teste de significância
  if (abs(Z) > regiao_critica) {
    print(paste("Rejeitar H0 ( Diferença significativa entre as proporções de Mulheres e Homens - Nível", 100*(1-alpha), ")"))
  } else {
    print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções de Mulheres e Homens - Nível", 100*(1-alpha), ")"))
  }
}




## Orientação

heteros = subset(sociodemo, socioidgenero == "Mulher (Cis)" | socioidgenero == "Homem (Cis)")
heteros = subset(heteros, socioorient == "het")

lgbt <- anti_join(sociodemo, heteros, by = "responseId")

# Heteros
dados_comb = heteros
dados_comb$Coluna_Combinada <- paste(heteros$teste01, heteros$teste02, heteros$teste03, sep = ",")

dados_comb = heteros
dados_comb$Coluna_Combinada <- paste(heteros$teste04, heteros$teste05, heteros$teste06,
                                     heteros$teste07, heteros$teste08, sep = ",")

dados_comb = heteros
dados_comb$Coluna_Combinada <- paste(heteros$teste09, heteros$teste10, heteros$teste11,
                                     heteros$teste12, heteros$teste13, heteros$teste14,
                                     heteros$teste15, sep = ",")

dados_comb = heteros
dados_comb$Coluna_Combinada <- paste(heteros$teste16, heteros$teste17, heteros$teste18,
                                     heteros$teste19, heteros$teste20, heteros$teste21,
                                     heteros$teste22, heteros$teste23, heteros$teste24,
                                     heteros$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Respondentes Heteros - Testes 16 a 25",
       x = "Respostas",
       y = "Contagem") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink",
                               "Apenas Machos" = "blue",
                               "Machos e Fêmeas" = "purple",
                               "Apenas Mulheres" = "red",
                               "Apenas Homens" = "green",
                               "Homens e Mulheres" = "yellow")) +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta a legenda de cores

# Exiba o gráfico de histograma
print(grafico_histograma_comb)

# Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
ggsave(filename = paste("Heteros/Categoria 4.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)


# LGBT
dados_comb = lgbt
dados_comb$Coluna_Combinada <- paste(lgbt$teste01, lgbt$teste02, lgbt$teste03, sep = ",")

dados_comb = lgbt
dados_comb$Coluna_Combinada <- paste(lgbt$teste04, lgbt$teste05, lgbt$teste06,
                                     lgbt$teste07, lgbt$teste08, sep = ",")

dados_comb = lgbt
dados_comb$Coluna_Combinada <- paste(lgbt$teste09, lgbt$teste10, lgbt$teste11,
                                     lgbt$teste12, lgbt$teste13, lgbt$teste14,
                                     lgbt$teste15, sep = ",")

dados_comb = lgbt
dados_comb$Coluna_Combinada <- paste(lgbt$teste16, lgbt$teste17, lgbt$teste18,
                                     lgbt$teste19, lgbt$teste20, lgbt$teste21,
                                     lgbt$teste22, lgbt$teste23, lgbt$teste24,
                                     lgbt$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Respondentes LGBT - Testes 16 a 25",
       x = "Respostas",
       y = "Contagem") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink",
                               "Apenas Machos" = "blue",
                               "Machos e Fêmeas" = "purple",
                               "Apenas Mulheres" = "red",
                               "Apenas Homens" = "green",
                               "Homens e Mulheres" = "yellow")) +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta a legenda de cores

# Exiba o gráfico de histograma
print(grafico_histograma_comb)

# Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
ggsave(filename = paste("LGBT/Categoria 4.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)


# Heteros

# Selecionando apenas as colunas do banco de dados referente aos testes
heteros = heteros[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(heteros)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
heteros <- heteros[, indices_colunas_impares]

# Renomeando os valores para deixar as colunas binárias
heteros[,c(1:3)] <- as.data.frame(lapply(heteros[,c(1:3)], 
                                              function(x) ifelse( x!= "Apenas Fêmeas", 
                                                                  "Outra Resposta", x)))

heteros[,c(4:8)] <- as.data.frame(lapply(heteros[,c(4:8)], 
                                              function(x) ifelse( x!= "Machos e Fêmeas", 
                                                                  "Outra Resposta", x)))

heteros[,c(9:15)] <- as.data.frame(lapply(heteros[,c(9:15)], 
                                               function(x) ifelse( x!= "Apenas Mulheres", 
                                                                   "Outra Resposta", x)))

heteros[,c(16:25)] <- as.data.frame(lapply(heteros[,c(16:25)], 
                                                function(x) ifelse( x!= "Homens e Mulheres", 
                                                                    "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
heteros_categoria_um_p = sum(heteros[c(1:3)] == "Apenas Fêmeas")/(3*nrow(heteros))
heteros_categoria_dois_p = sum(heteros[c(4:8)] == "Machos e Fêmeas")/(5*nrow(heteros))
heteros_categoria_tres_p = sum(heteros[c(9:15)] == "Apenas Mulheres")/(7*nrow(heteros))
heteros_categoria_quatro_p = sum(heteros[c(16:25)] == "Homens e Mulheres")/(10*nrow(heteros))

categorias = c(heteros_categoria_um_p, heteros_categoria_dois_p,
               heteros_categoria_tres_p, heteros_categoria_quatro_p)

heteros_categorias = categorias

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(heteros), 4))

heteros_tamanho_categoria = tamanho_categoria

for (i in 1:3) {
  for (j in (i+1):4){
    categoria_i_p = categorias[i]
    categoria_i_1_p = 1 - categorias[i]
    categoria_j_p = categorias[j]
    categoria_j_1_p = 1 - categorias[j]
    
    # Estatística do Teste
    Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / tamanho_categoria[j]))
    
    # Testes comparados
    print(paste("Categoria", i, "X Categoria", j))
    
    # Proporções
    print(paste("Proporção da Categoria", i, "=", round(categoria_i_p, digits = 3)))
    print(paste("Proporção da Categoria", j, "=", round(categoria_j_p, digits = 3)))
    
    # Tamanho das amostras
    #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
    #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
    
    # Teste de significância
    if (abs(Z) > regiao_critica) {
      print(paste("Rejeitar H0 ( Diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    } else {
      print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    }
  }
}

# LGBT

# Selecionando apenas as colunas do banco de dados referente aos testes
lgbt = lgbt[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(lgbt)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
lgbt <- lgbt[, indices_colunas_impares]

# Renomeando os valores para deixar as colunas binárias
lgbt[,c(1:3)] <- as.data.frame(lapply(lgbt[,c(1:3)], 
                                            function(x) ifelse( x!= "Apenas Fêmeas", 
                                                                "Outra Resposta", x)))

lgbt[,c(4:8)] <- as.data.frame(lapply(lgbt[,c(4:8)], 
                                            function(x) ifelse( x!= "Machos e Fêmeas", 
                                                                "Outra Resposta", x)))

lgbt[,c(9:15)] <- as.data.frame(lapply(lgbt[,c(9:15)], 
                                             function(x) ifelse( x!= "Apenas Mulheres", 
                                                                 "Outra Resposta", x)))

lgbt[,c(16:25)] <- as.data.frame(lapply(lgbt[,c(16:25)], 
                                              function(x) ifelse( x!= "Homens e Mulheres", 
                                                                  "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
lgbt_categoria_um_p = sum(lgbt[c(1:3)] == "Apenas Fêmeas")/(3*nrow(lgbt))
lgbt_categoria_dois_p = sum(lgbt[c(4:8)] == "Machos e Fêmeas")/(5*nrow(lgbt))
lgbt_categoria_tres_p = sum(lgbt[c(9:15)] == "Apenas Mulheres")/(7*nrow(lgbt))
lgbt_categoria_quatro_p = sum(lgbt[c(16:25)] == "Homens e Mulheres")/(10*nrow(lgbt))

categorias = c(lgbt_categoria_um_p, lgbt_categoria_dois_p,
               lgbt_categoria_tres_p, lgbt_categoria_quatro_p)

lgbt_categorias = categorias

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(lgbt), 4))

lgbt_tamanho_categoria = tamanho_categoria

for (i in 1:3) {
  for (j in (i+1):4){
    categoria_i_p = categorias[i]
    categoria_i_1_p = 1 - categorias[i]
    categoria_j_p = categorias[j]
    categoria_j_1_p = 1 - categorias[j]
    
    # Estatística do Teste
    Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / tamanho_categoria[j]))
    
    # Testes comparados
    print(paste("Categoria", i, "X Categoria", j))
    
    # Proporções
    print(paste("Proporção da Categoria", i, "=", round(categoria_i_p, digits = 3)))
    print(paste("Proporção da Categoria", j, "=", round(categoria_j_p, digits = 3)))
    
    # Tamanho das amostras
    #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
    #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
    
    # Teste de significância
    if (abs(Z) > regiao_critica) {
      print(paste("Rejeitar H0 ( Diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    } else {
      print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    }
  }
}


# Testes de Hipóteses comparando as proporções mais prevalentes entre homens e 
# mulheres de cada uma das 4 categorias

for (i in 1:4) {
  
  categoria_i_p = heteros_categorias[i]
  categoria_i_1_p = 1 - heteros_categorias[i]
  categoria_j_p = lgbt_categorias[i]
  categoria_j_1_p = 1 - lgbt_categorias[i]
  
  # Estatística do Teste
  Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / heteros_tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / lgbt_tamanho_categoria[i]))
  
  # Testes comparados
  print(paste("Categoria", i))
  
  # Proporções
  print(paste("Proporção de Heteros =", round(categoria_i_p, digits = 3)))
  print(paste("Proporção de LGBTs =", round(categoria_j_p, digits = 3)))
  
  # Tamanho das amostras
  #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
  #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
  
  # Teste de significância
  if (abs(Z) > regiao_critica) {
    print(paste("Rejeitar H0 ( Diferença significativa entre as proporções de Heteros e LGBTs - Nível", 100*(1-alpha), ")"))
  } else {
    print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções de Heteros e LGBTs - Nível", 100*(1-alpha), ")"))
  }
}




## Renda

baixa_renda = subset(sociodemo, sociorenda == "n" | sociorenda == "0.5" | sociorenda == "0.5_1" |
                      sociorenda == "1_2" | sociorenda == "2_3" | sociorenda == "3_5")
#alta_renda = subset(sociodemo, sociorenda == "5_10" | sociorenda == "10_20" | sociorenda == "20+")
alta_renda <- anti_join(sociodemo, baixa_renda, by = "responseId")

# Baixa Renda
dados_comb = baixa_renda
dados_comb$Coluna_Combinada <- paste(baixa_renda$teste01, baixa_renda$teste02, baixa_renda$teste03, sep = ",")

dados_comb = baixa_renda
dados_comb$Coluna_Combinada <- paste(baixa_renda$teste04, baixa_renda$teste05, baixa_renda$teste06,
                                     baixa_renda$teste07, baixa_renda$teste08, sep = ",")

dados_comb = baixa_renda
dados_comb$Coluna_Combinada <- paste(baixa_renda$teste09, baixa_renda$teste10, baixa_renda$teste11,
                                     baixa_renda$teste12, baixa_renda$teste13, baixa_renda$teste14,
                                     baixa_renda$teste15, sep = ",")

dados_comb = baixa_renda
dados_comb$Coluna_Combinada <- paste(baixa_renda$teste16, baixa_renda$teste17, baixa_renda$teste18,
                                     baixa_renda$teste19, baixa_renda$teste20, baixa_renda$teste21,
                                     baixa_renda$teste22, baixa_renda$teste23, baixa_renda$teste24,
                                     baixa_renda$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Respondentes Até 5 S.M. - Testes 16 a 25",
       x = "Respostas",
       y = "Contagem") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink",
                               "Apenas Machos" = "blue",
                               "Machos e Fêmeas" = "purple",
                               "Apenas Mulheres" = "red",
                               "Apenas Homens" = "green",
                               "Homens e Mulheres" = "yellow")) +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta a legenda de cores

# Exiba o gráfico de histograma
print(grafico_histograma_comb)

# Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
ggsave(filename = paste("Até 5 S.M./Categoria 4.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)


# Alta Renda
dados_comb = alta_renda
dados_comb$Coluna_Combinada <- paste(alta_renda$teste01, alta_renda$teste02, alta_renda$teste03, sep = ",")

dados_comb = alta_renda
dados_comb$Coluna_Combinada <- paste(alta_renda$teste04, alta_renda$teste05, alta_renda$teste06,
                                     alta_renda$teste07, alta_renda$teste08, sep = ",")

dados_comb = alta_renda
dados_comb$Coluna_Combinada <- paste(alta_renda$teste09, alta_renda$teste10, alta_renda$teste11,
                                     alta_renda$teste12, alta_renda$teste13, alta_renda$teste14,
                                     alta_renda$teste15, sep = ",")

dados_comb = alta_renda
dados_comb$Coluna_Combinada <- paste(alta_renda$teste16, alta_renda$teste17, alta_renda$teste18,
                                     alta_renda$teste19, alta_renda$teste20, alta_renda$teste21,
                                     alta_renda$teste22, alta_renda$teste23, alta_renda$teste24,
                                     alta_renda$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Respondentes Acima de 5 S.M. - Testes 16 a 25",
       x = "Respostas",
       y = "Contagem") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink",
                               "Apenas Machos" = "blue",
                               "Machos e Fêmeas" = "purple",
                               "Apenas Mulheres" = "red",
                               "Apenas Homens" = "green",
                               "Homens e Mulheres" = "yellow")) +
  theme_minimal() +
  theme(legend.position = "none")  # Oculta a legenda de cores

# Exiba o gráfico de histograma
print(grafico_histograma_comb)

# Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
ggsave(filename = paste("Acima de 5 S.M./Categoria 4.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)


# Baixa Renda

# Selecionando apenas as colunas do banco de dados referente aos testes
baixa_renda = baixa_renda[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(baixa_renda)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
baixa_renda <- baixa_renda[, indices_colunas_impares]

# Renomeando os valores para deixar as colunas binárias
baixa_renda[,c(1:3)] <- as.data.frame(lapply(baixa_renda[,c(1:3)], 
                                         function(x) ifelse( x!= "Apenas Fêmeas", 
                                                             "Outra Resposta", x)))

baixa_renda[,c(4:8)] <- as.data.frame(lapply(baixa_renda[,c(4:8)], 
                                         function(x) ifelse( x!= "Machos e Fêmeas", 
                                                             "Outra Resposta", x)))

baixa_renda[,c(9:15)] <- as.data.frame(lapply(baixa_renda[,c(9:15)], 
                                          function(x) ifelse( x!= "Apenas Mulheres", 
                                                              "Outra Resposta", x)))

baixa_renda[,c(16:25)] <- as.data.frame(lapply(baixa_renda[,c(16:25)], 
                                           function(x) ifelse( x!= "Homens e Mulheres", 
                                                               "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
baixa_renda_categoria_um_p = sum(baixa_renda[c(1:3)] == "Apenas Fêmeas")/(3*nrow(baixa_renda))
baixa_renda_categoria_dois_p = sum(baixa_renda[c(4:8)] == "Machos e Fêmeas")/(5*nrow(baixa_renda))
baixa_renda_categoria_tres_p = sum(baixa_renda[c(9:15)] == "Apenas Mulheres")/(7*nrow(baixa_renda))
baixa_renda_categoria_quatro_p = sum(baixa_renda[c(16:25)] == "Homens e Mulheres")/(10*nrow(baixa_renda))

categorias = c(baixa_renda_categoria_um_p, baixa_renda_categoria_dois_p,
               baixa_renda_categoria_tres_p, baixa_renda_categoria_quatro_p)

baixa_renda_categorias = categorias

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(baixa_renda), 4))

baixa_renda_tamanho_categoria = tamanho_categoria

for (i in 1:3) {
  for (j in (i+1):4){
    categoria_i_p = categorias[i]
    categoria_i_1_p = 1 - categorias[i]
    categoria_j_p = categorias[j]
    categoria_j_1_p = 1 - categorias[j]
    
    # Estatística do Teste
    Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / tamanho_categoria[j]))
    
    # Testes comparados
    print(paste("Categoria", i, "X Categoria", j))
    
    # Proporções
    print(paste("Proporção da Categoria", i, "=", round(categoria_i_p, digits = 3)))
    print(paste("Proporção da Categoria", j, "=", round(categoria_j_p, digits = 3)))
    
    # Tamanho das amostras
    #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
    #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
    
    # Teste de significância
    if (abs(Z) > regiao_critica) {
      print(paste("Rejeitar H0 ( Diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    } else {
      print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    }
  }
}

# Alta Renda

# Selecionando apenas as colunas do banco de dados referente aos testes
alta_renda = alta_renda[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(alta_renda)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
alta_renda <- alta_renda[, indices_colunas_impares]

# Renomeando os valores para deixar as colunas binárias
alta_renda[,c(1:3)] <- as.data.frame(lapply(alta_renda[,c(1:3)], 
                                      function(x) ifelse( x!= "Apenas Fêmeas", 
                                                          "Outra Resposta", x)))

alta_renda[,c(4:8)] <- as.data.frame(lapply(alta_renda[,c(4:8)], 
                                      function(x) ifelse( x!= "Machos e Fêmeas", 
                                                          "Outra Resposta", x)))

alta_renda[,c(9:15)] <- as.data.frame(lapply(alta_renda[,c(9:15)], 
                                       function(x) ifelse( x!= "Apenas Mulheres", 
                                                           "Outra Resposta", x)))

alta_renda[,c(16:25)] <- as.data.frame(lapply(alta_renda[,c(16:25)], 
                                        function(x) ifelse( x!= "Homens e Mulheres", 
                                                            "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
alta_renda_categoria_um_p = sum(alta_renda[c(1:3)] == "Apenas Fêmeas")/(3*nrow(alta_renda))
alta_renda_categoria_dois_p = sum(alta_renda[c(4:8)] == "Machos e Fêmeas")/(5*nrow(alta_renda))
alta_renda_categoria_tres_p = sum(alta_renda[c(9:15)] == "Apenas Mulheres")/(7*nrow(alta_renda))
alta_renda_categoria_quatro_p = sum(alta_renda[c(16:25)] == "Homens e Mulheres")/(10*nrow(alta_renda))

categorias = c(alta_renda_categoria_um_p, alta_renda_categoria_dois_p,
               alta_renda_categoria_tres_p, alta_renda_categoria_quatro_p)

alta_renda_categorias = categorias

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(alta_renda), 4))

alta_renda_tamanho_categoria = tamanho_categoria

for (i in 1:3) {
  for (j in (i+1):4){
    categoria_i_p = categorias[i]
    categoria_i_1_p = 1 - categorias[i]
    categoria_j_p = categorias[j]
    categoria_j_1_p = 1 - categorias[j]
    
    # Estatística do Teste
    Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / tamanho_categoria[j]))
    
    # Testes comparados
    print(paste("Categoria", i, "X Categoria", j))
    
    # Proporções
    print(paste("Proporção da Categoria", i, "=", round(categoria_i_p, digits = 3)))
    print(paste("Proporção da Categoria", j, "=", round(categoria_j_p, digits = 3)))
    
    # Tamanho das amostras
    #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
    #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
    
    # Teste de significância
    if (abs(Z) > regiao_critica) {
      print(paste("Rejeitar H0 ( Diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    } else {
      print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções - Nível", 100*(1-alpha), ")"))
    }
  }
}


# Testes de Hipóteses comparando as proporções mais prevalentes entre homens e 
# mulheres de cada uma das 4 categorias

for (i in 1:4) {
  
  categoria_i_p = baixa_renda_categorias[i]
  categoria_i_1_p = 1 - baixa_renda_categorias[i]
  categoria_j_p = alta_renda_categorias[i]
  categoria_j_1_p = 1 - alta_renda_categorias[i]
  
  # Estatística do Teste
  Z = (categoria_i_p - categoria_j_p) / sqrt(((categoria_i_p * categoria_i_1_p) / baixa_renda_tamanho_categoria[i]) + ((categoria_j_p * categoria_j_1_p) / alta_renda_tamanho_categoria[i]))

  # Testes comparados
  print(paste("Categoria", i))
  
  # Proporções
  print(paste("Proporção de Pessoas Até 5 S.M. =", round(categoria_i_p, digits = 3)))
  print(paste("Proporção de Pessoas Acima de 5 S.M. =", round(categoria_j_p, digits = 3)))
  
  # Tamanho das amostras
  #print(paste("Tamanho da Amostra", i, "=", round(tamanho_categoria[i], digits = 3)))
  #print(paste("Tamanho da Amostra", j, "=", round(tamanho_categoria[j], digits = 3)))
  
  # Teste de significância
  if (abs(Z) > regiao_critica) {
    print(paste("Rejeitar H0 ( Diferença significativa entre as proporções de Pessoas Até 5 S.M. e Pessoas Acima de 5 S.M. - Nível", 100*(1-alpha), ")"))
  } else {
    print(paste("Não rejeitar H0 ( Sem diferença significativa entre as proporções de Pessoas Até 5 S.M. e Pessoas Acima de 5 S.M. - Nível", 100*(1-alpha), ")"))
  }
}



