
### Proporções ###

# Selecionando apenas as colunas do banco de dados referente aos testes
proporçoes = banco_dados_atualizado[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(proporçoes)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
proporçoes <- proporçoes[, indices_colunas_impares]

# Renomear os valores
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("neut.ani", "Machos e Fêmeas", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("neut.hum", "Homens e Mulheres", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.ani.macho", "Apenas Machos", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.ani.femea", "Apenas Fêmeas", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.hum.homem", "Apenas Homens", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.hum.mul", "Apenas Mulheres", x)))


# Renomeando os valores para deixar as colunas binárias
proporçoes[,c(1:3)] <- as.data.frame(lapply(proporçoes[,c(1:3)], 
                                            function(x) ifelse( x!= "Apenas Fêmeas", 
                                                                "Outra Resposta", x)))

proporçoes[,c(4:8)] <- as.data.frame(lapply(proporçoes[,c(4:8)], 
                                            function(x) ifelse( x!= "Apenas Machos", 
                                                                "Outra Resposta", x)))

proporçoes[,c(9:15)] <- as.data.frame(lapply(proporçoes[,c(9:15)], 
                                            function(x) ifelse( x!= "Apenas Mulheres", 
                                                                "Outra Resposta", x)))

proporçoes[,c(16:25)] <- as.data.frame(lapply(proporçoes[,c(16:25)], 
                                            function(x) ifelse( x!= "Apenas Homens", 
                                                                "Outra Resposta", x)))

# Testes de Hipóteses para proporções
table(proporçoes$teste01)[1]/nrow(proporçoes)
table(proporçoes$teste01)[2]/nrow(proporçoes)

# Nível de Confiança
alpha = 0.05

# Valor crítico para um teste bilateral com alfa = 0.05
regiao_critica = qnorm(1 - alpha/2)
  
for (i in 1:25) {
  if(i<3){
    for (j in (i+1):3) {
      ani_fem_teste_i_p = sum(proporçoes[[i]] == "Apenas Fêmeas")/nrow(proporçoes)
      ani_fem_teste_i_1_p = sum(proporçoes[[i]] != "Apenas Fêmeas")/nrow(proporçoes)
      ani_fem_teste_j_p = sum(proporçoes[[j]] == "Apenas Fêmeas")/nrow(proporçoes)
      ani_fem_teste_j_1_p = sum(proporçoes[[j]] != "Apenas Fêmeas")/nrow(proporçoes)
      
      # Estatística do Teste
      Z = (ani_fem_teste_i_p - ani_fem_teste_j_p) / sqrt((ani_fem_teste_i_p * ani_fem_teste_i_1_p + ani_fem_teste_j_p * ani_fem_teste_j_1_p)/nrow(proporçoes))
      
      # Testes comparados
      print(paste("Teste", i, "X Teste", j))
      
      # Teste de significância
      if (abs(Z) > regiao_critica) {
        print("Rejeitar H0 (Diferença significativa entre as proporções - Nível 95%)")
      } else {
        print("Não rejeitar H0 (Sem diferença significativa entre as proporções - Nível 95%)")
      }
    }
  }
  if(i>3 && i<8){
    for (j in (i+1):8) {
      ani_fem_teste_i_p = sum(proporçoes[[i]] == "Apenas Machos")/nrow(proporçoes)
      ani_fem_teste_i_1_p = sum(proporçoes[[i]] != "Apenas Machos")/nrow(proporçoes)
      ani_fem_teste_j_p = sum(proporçoes[[j]] == "Apenas Machos")/nrow(proporçoes)
      ani_fem_teste_j_1_p = sum(proporçoes[[j]] != "Apenas Machos")/nrow(proporçoes)
      
      # Estatística do Teste
      Z = (ani_fem_teste_i_p - ani_fem_teste_j_p) / sqrt((ani_fem_teste_i_p * ani_fem_teste_i_1_p + ani_fem_teste_j_p * ani_fem_teste_j_1_p)/nrow(proporçoes))
      
      # Testes comparados
      print(paste("Teste", i, "X Teste", j))
      
      # Teste de significância
      if (abs(Z) > regiao_critica) {
        print("Rejeitar H0 (Diferença significativa entre as proporções - Nível 95%)")
      } else {
        print("Não rejeitar H0 (Sem diferença significativa entre as proporções - Nível 95%)")
      }
    }
  }
  if(i>8 && i<15){
    for (j in (i+1):15) {
      ani_fem_teste_i_p = sum(proporçoes[[i]] == "Apenas Mulheres")/nrow(proporçoes)
      ani_fem_teste_i_1_p = sum(proporçoes[[i]] != "Apenas Mulheres")/nrow(proporçoes)
      ani_fem_teste_j_p = sum(proporçoes[[j]] == "Apenas Mulheres")/nrow(proporçoes)
      ani_fem_teste_j_1_p = sum(proporçoes[[j]] != "Apenas Mulheres")/nrow(proporçoes)
      
      # Estatística do Teste
      Z = (ani_fem_teste_i_p - ani_fem_teste_j_p) / sqrt((ani_fem_teste_i_p * ani_fem_teste_i_1_p + ani_fem_teste_j_p * ani_fem_teste_j_1_p)/nrow(proporçoes))
      
      # Testes comparados
      print(paste("Teste", i, "X Teste", j))
      
      # Teste de significância
      if (abs(Z) > regiao_critica) {
        print("Rejeitar H0 (Diferença significativa entre as proporções - Nível 95%)")
      } else {
        print("Não rejeitar H0 (Sem diferença significativa entre as proporções - Nível 95%)")
      }
    }
  }
  if(i>15 && i<25){
    for (j in (i+1):25) {
      ani_fem_teste_i_p = sum(proporçoes[[i]] == "Apenas Homens")/nrow(proporçoes)
      ani_fem_teste_i_1_p = sum(proporçoes[[i]] != "Apenas Homens")/nrow(proporçoes)
      ani_fem_teste_j_p = sum(proporçoes[[j]] == "Apenas Homens")/nrow(proporçoes)
      ani_fem_teste_j_1_p = sum(proporçoes[[j]] != "Apenas Homens")/nrow(proporçoes)
      
      # Estatística do Teste
      Z = (ani_fem_teste_i_p - ani_fem_teste_j_p) / sqrt((ani_fem_teste_i_p * ani_fem_teste_i_1_p + ani_fem_teste_j_p * ani_fem_teste_j_1_p)/nrow(proporçoes))
      
      # Testes comparados
      print(paste("Teste", i, "X Teste", j))
      
      # Teste de significância
      if (abs(Z) > regiao_critica) {
        print("Rejeitar H0 (Diferença significativa entre as proporções - Nível 95%)")
      } else {
        print("Não rejeitar H0 (Sem diferença significativa entre as proporções - Nível 95%)")
      }
    }
  }
}


# Anteriormente, havia feito os testes em relação aos gêneros das frases, porém
# foi decidido fazer os testes de acordo com as respostas mais prevalentes

# Selecionando apenas as colunas do banco de dados referente aos testes
proporçoes = banco_dados_atualizado[,c(23:72)]

# Obtenha o número total de colunas no dataframe
num_colunas <- ncol(proporçoes)

# Crie um vetor de índices para as colunas pares
indices_colunas_impares <- seq(1, num_colunas, by = 2)

# Crie um subconjunto do dataframe com as colunas pares
proporçoes <- proporçoes[, indices_colunas_impares]

# Renomear os valores
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("neut.ani", "Machos e Fêmeas", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("neut.hum", "Homens e Mulheres", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.ani.macho", "Apenas Machos", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.ani.femea", "Apenas Fêmeas", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.hum.homem", "Apenas Homens", x)))
proporçoes <- as.data.frame(lapply(proporçoes, function(x) gsub("exc.hum.mul", "Apenas Mulheres", x)))


# Renomeando os valores para deixar as colunas binárias
proporçoes[,c(1:3)] <- as.data.frame(lapply(proporçoes[,c(1:3)], 
                                            function(x) ifelse( x!= "Apenas Fêmeas", 
                                                                "Outra Resposta", x)))

proporçoes[,c(4:8)] <- as.data.frame(lapply(proporçoes[,c(4:8)], 
                                            function(x) ifelse( x!= "Machos e Fêmeas", 
                                                                "Outra Resposta", x)))

proporçoes[,c(9:15)] <- as.data.frame(lapply(proporçoes[,c(9:15)], 
                                             function(x) ifelse( x!= "Apenas Mulheres", 
                                                                 "Outra Resposta", x)))

proporçoes[,c(16:25)] <- as.data.frame(lapply(proporçoes[,c(16:25)], 
                                              function(x) ifelse( x!= "Homens e Mulheres", 
                                                                  "Outra Resposta", x)))

# Proporções das categorias mais prevalentes
ani_fem_categoria_um_p = sum(proporçoes[c(1:3)] == "Apenas Fêmeas")/(3*nrow(proporçoes))
ani_mas_categoria_dois_p = sum(proporçoes[c(4:8)] == "Machos e Fêmeas")/(5*nrow(proporçoes))
hum_fem_categoria_tres_p = sum(proporçoes[c(9:15)] == "Apenas Mulheres")/(7*nrow(proporçoes))
hum_mas_categoria_quatro_p = sum(proporçoes[c(16:25)] == "Homens e Mulheres")/(10*nrow(proporçoes))

categorias = c(ani_fem_categoria_um_p, ani_mas_categoria_dois_p,
               hum_fem_categoria_tres_p, hum_mas_categoria_quatro_p)

#tamanho_categoria = c(3*nrow(proporçoes), 5*nrow(proporçoes), 7*nrow(proporçoes), 10*nrow(proporçoes))
tamanho_categoria = c(rep(nrow(proporçoes), 4))

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







p_ani_mas
p_hum_fem
p_hum_mas
