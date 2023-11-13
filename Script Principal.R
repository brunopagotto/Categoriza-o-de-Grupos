### Monografia Lucas - Script Principal ###

library(readr)
library(ggplot2)
library(dplyr)
install.packages("writexl")
library(writexl)

dados_pavlovia <- read_csv("Dados (final) - cidades ajustadas.csv")
View(dados_pavlovia)

# Excluir colunas indicadas no email
colunas_para_excluir <- c("email", "email_rt", "responseDate", "isComplete",
                          "surveyStatus", "length_ch", "expanded", "fbclid")
dados <- dados_pavlovia[, !(names(dados_pavlovia) %in% colunas_para_excluir)]

# Excluir linhas dos que não consentiram
table(dados$consentimento)
dados = subset(dados, consentimento != "n")

# Remover participantes com ensino superior incompleto ou acima das áreas de 
# Letras, Linguística, Estudos Literários, Fonoaudiologia, Tradução, etc
cursos = data.frame(table(dados$socioescolasuperior))
View(cursos)
write_xlsx(cursos, path = "cursos.xlsx")

# Padronizar a escrita da coluna 'socioescolasuperior', deixando tudo com letra 
# minúscula, sem acento, sem caracteres especiais e substituindo pontuação por
# espaço
dados$socioescolasuperior <- tolower(dados$socioescolasuperior)
dados$socioescolasuperior <- iconv(dados$socioescolasuperior, to = "ASCII//TRANSLIT")
dados$socioescolasuperior <- gsub("ç", "c", dados$socioescolasuperior)
dados$socioescolasuperior <- gsub("[[:punct:]]", " ", dados$socioescolasuperior)

#cursos_para_excluir = c("letras", "linguistica", "estudos literarios",
#                        "fonoaudiologia", "traducao", "teoria literaria")

tem_letras <- grepl("letras", dados$socioescolasuperior, ignore.case = TRUE)
table(tem_letras)
dados <- dados[!tem_letras, ]

tem_lingu <- grepl("lingu", dados$socioescolasuperior, ignore.case = TRUE)
table(tem_lingu)
dados <- dados[!tem_lingu, ]

tem_liter <- grepl("liter", dados$socioescolasuperior, ignore.case = TRUE)
table(tem_liter)
dados <- dados[!tem_liter, ]

tem_fonoaudiologia <- grepl("fonoaudiologia", dados$socioescolasuperior, ignore.case = TRUE)
table(tem_fonoaudiologia)
dados <- dados[!tem_fonoaudiologia, ]

tem_traducao <- grepl("traducao", dados$socioescolasuperior, ignore.case = TRUE)
table(tem_traducao)
dados <- dados[!tem_traducao, ]

# Padronizar a escrita das colunas 'sociomora' e 'socionasc', deixando tudo com
# letra minúscula, sem acento, sem caracteres especiais e substituindo pontuação
# por espaço
dados$sociomora <- tolower(dados$sociomora)
dados$sociomora <- iconv(dados$sociomora, to = "ASCII//TRANSLIT")
dados$sociomora <- gsub("ç", "c", dados$sociomora)
dados$sociomora <- gsub("[[:punct:]]", " ", dados$sociomora)

# socionasc
dados$socionasc <- tolower(dados$socionasc)
dados$socionasc <- iconv(dados$socionasc, to = "ASCII//TRANSLIT")
dados$socionasc <- gsub("ç", "c", dados$socionasc)
dados$socionasc <- gsub("[[:punct:]]", " ", dados$socionasc)

# Todas as cidades e seus respectivos estados extraídos de uma biblioteca do Python chamada ufbr 
# (https://pt.linkedin.com/pulse/estados-e-munic%C3%ADpios-brasileiros-em-python-sidon-duarte)
municipios_brasil <- read_csv("municipios_brasil.csv")

# Exporte o DataFrame para um arquivo Excel
#write_xlsx(municipios_brasil, "municipios_brasil.xlsx")

# Mudar o nome da coluna em municipios_brasil para facilitar o Inner Join
municipios_brasil$sociomora = municipios_brasil$Cidade
municipios_brasil$socionasc = municipios_brasil$Cidade

# Padronizar a escrita das colunas 'sociomora' e 'socionasc', deixando tudo com
# letra minúscula, sem acento, sem caracteres especiais e substituindo pontuação
# por espaço
municipios_brasil$sociomora <- tolower(municipios_brasil$sociomora)
municipios_brasil$sociomora <- iconv(municipios_brasil$sociomora, to = "ASCII//TRANSLIT")
municipios_brasil$sociomora <- gsub("ç", "c", municipios_brasil$sociomora)
municipios_brasil$sociomora <- gsub("[[:punct:]]", " ", municipios_brasil$sociomora)

# Alterar nomes que vieram errado do Python
municipios_brasil$sociomora = gsub("moji mirim", "mogi mirim", municipios_brasil$sociomora)
municipios_brasil$sociomora = gsub("embu", "embu das artes", municipios_brasil$sociomora)
municipios_brasil$sociomora = gsub("diamante d oeste", "diamante do oeste", municipios_brasil$sociomora)

# Padronizar a escrita das colunas 'sociomora' e 'socionasc', deixando tudo com
# letra minúscula, sem acento, sem caracteres especiais e substituindo pontuação
# por espaço
municipios_brasil$socionasc <- tolower(municipios_brasil$socionasc)
municipios_brasil$socionasc <- iconv(municipios_brasil$socionasc, to = "ASCII//TRANSLIT")
municipios_brasil$socionasc <- gsub("ç", "c", municipios_brasil$socionasc)
municipios_brasil$socionasc <- gsub("[[:punct:]]", " ", municipios_brasil$socionasc)

# Alterar nomes que vieram errado do Python
municipios_brasil$socionasc = gsub("moji mirim", "mogi mirim", municipios_brasil$socionasc)
municipios_brasil$socionasc = gsub("embu", "embu das artes", municipios_brasil$socionasc)
municipios_brasil$socionasc = gsub("diamante d oeste", "diamante do oeste", municipios_brasil$socionasc)

# Realize um join baseado na coluna 'Cidade'
merged_df <- inner_join(dados[-c(2,4,5,76:85)], municipios_brasil, by = "sociomora")

# Encontrar cidades com nomes repetidos em estados diferentes
repeticoes_tempo_resposta = merged_df[duplicated(merged_df$consentimento_rt) | duplicated(merged_df$consentimento_rt, fromLast = TRUE), ]

# Encontre os valores únicos dentre as cidades com nomes repetidos em estados diferentes
valores_unicos <- unique(repeticoes_tempo_resposta$sociomora)

# Exiba os valores únicos
print(valores_unicos)

# Alterar nomes das cidades repetidas em diferentes estado no dataframe 
# municipios_brasil incluindo o estado ao lado para ficar da mesma forma
# que os dados foram padronizados manualmente
for (i in 1:length(valores_unicos)) {
  municipios_brasil$sociomora = ifelse(municipios_brasil$sociomora == valores_unicos[i], sprintf("%s %s", municipios_brasil$sociomora, municipios_brasil$Estado), municipios_brasil$sociomora)
}
municipios_brasil$sociomora <- tolower(municipios_brasil$sociomora)

#repeticoes_tempo_resposta$sociomora_Estado <- sprintf("%s %s", repeticoes_tempo_resposta$sociomora, repeticoes_tempo_resposta$Estado)
#valores_unicos_estado = unique(repeticoes_tempo_resposta$sociomora_Estado)
#print(valores_unicos_estado)

# Realize um join baseado na coluna 'Cidade'
merged_df <- inner_join(dados[-c(2,4,5,76:85)], municipios_brasil, by = "sociomora")

# Para encontrar as linhas que não foram incluídas no resultado do inner join
resultado_anti <- anti_join(dados[-c(2,4,5,76:85)], merged_df, by = "sociomora")

# Substituir cidade em sociomora por estado
merged_df$sociomora = merged_df$Estado
merged_df = merged_df[-c(73,74)]



# Realize um join baseado na coluna 'Cidade'
#df_aux <- inner_join(merged_df, municipios_brasil, by = "socionasc")

# Realize um join baseado na coluna 'Cidade'
df_aux <- inner_join(dados[-c(2,4,5,76:85)], municipios_brasil, by = "socionasc")

# Encontrar cidades com nomes repetidos em estados diferentes
repeticoes_tempo_resposta = df_aux[duplicated(df_aux$consentimento_rt) | duplicated(df_aux$consentimento_rt, fromLast = TRUE), ]

# Encontre os valores únicos dentre as cidades com nomes repetidos em estados diferentes
valores_unicos <- unique(repeticoes_tempo_resposta$socionasc)

# Exiba os valores únicos
print(valores_unicos)

# Encontrar o número de casos de cada cidade
for (i in 1:length(valores_unicos)) {
  print(valores_unicos[i])
  print(table(repeticoes_tempo_resposta$socionasc == valores_unicos[i]))
}

# Alterar nomes das cidades repetidas em diferentes estado no dataframe 
# municipios_brasil incluindo o estado ao lado para ficar da mesma forma
# que os dados foram padronizados manualmente
municipios_brasil$socionasc = municipios_brasil$sociomora
for (i in 1:length(valores_unicos)) {
  municipios_brasil$socionasc = ifelse(municipios_brasil$socionasc == valores_unicos[i], sprintf("%s %s", municipios_brasil$socionasc, municipios_brasil$Estado), municipios_brasil$socionasc)
}
municipios_brasil$socionasc <- tolower(municipios_brasil$socionasc)

# Substituir escritas erradas pelos participantes
merged_df$socionasc = gsub("juruce", "jardinopolis", merged_df$socionasc)
merged_df$socionasc = gsub("joao pessoa", "jacunda", merged_df$socionasc)
merged_df$socionasc = gsub("pedermeiras", "pederneiras", merged_df$socionasc)

# Realize um join baseado na coluna 'Cidade'
merged_df$socionasc <- tolower(merged_df$socionasc)
df_aux <- inner_join(merged_df, municipios_brasil, by = "socionasc")

# Para encontrar as linhas que não foram incluídas no resultado do inner join
resultado_anti <- anti_join(merged_df, df_aux, by = "socionasc")

# Repassar o df_aux para o merge_df e excluir a linha juruce/jardinopolis
df_aux = df_aux[-670,]
merged_df = df_aux

# Substituir cidade em socionasc por estado
merged_df$socionasc = merged_df$Estado
merged_df = merged_df[-c(73,74,75)]
colnames(merged_df)[colnames(merged_df) == "sociomora.x"] <- "sociomora"

# Escrever o banco de dados após as atualizações
df_aux = df_aux[-c(73,74,75)]
colunas_exceto_estados = colnames(df_aux)
colunas_exceto_estados = colunas_exceto_estados[-c(13,15)]
banco_dados_atualizado = inner_join(df_aux, dados, by = c(colunas_exceto_estados))
banco_dados_atualizado = banco_dados_atualizado[-c(76,77)]
colnames(banco_dados_atualizado)[colnames(banco_dados_atualizado) == "sociomora.x"] <- "sociomora"
colnames(banco_dados_atualizado)[colnames(banco_dados_atualizado) == "socionasc.x"] <- "socionasc"

write_xlsx(banco_dados_atualizado, path = "Banco de Dados Atualizado.xlsx")
write.csv(banco_dados_atualizado, file = "Banco de Dados Atualizado.csv", row.names = FALSE)

# Tabelas e gráficos de escolaridade, identidade étnico-racial, faixa de idade,
# identidade de gênero, estado em que nasceu, estado em que mora, orientação
# sexual, pronome de preferência e faixa de renda domiciliar

# Usar os dados com menos colunas
banco_dados_atualizado = read_csv("Banco de Dados Atualizado.csv")
merged_df = banco_dados_atualizado

### Escolaridade

merged_df$socioescola = gsub("fund.inc", "Fundamental Incompleto", merged_df$socioescola)
merged_df$socioescola = gsub("fund.com", "Fundamental Completo", merged_df$socioescola)
merged_df$socioescola = gsub("med.inc", "Médio Incompleto", merged_df$socioescola)
merged_df$socioescola = gsub("med.com", "Médio Completo", merged_df$socioescola)
merged_df$socioescola = gsub("sup.inc", "Superior Incompleto", merged_df$socioescola)
merged_df$socioescola = gsub("sup.com", "Superior Completo", merged_df$socioescola)
merged_df$socioescola = gsub("sup.pos", "Pós-Graduação", merged_df$socioescola)

# Tabela
table(merged_df$socioescola)

# Define os níveis na ordem desejada
ordem_desejada <- c("Fundamental Incompleto","Fundamental Completo", "Médio Incompleto", 
                    "Médio Completo", "Superior Incompleto", "Superior Completo", "Pós-Graduação")

# Cria um fator com os níveis na ordem desejada
merged_df$socioescola <- factor(merged_df$socioescola, levels = ordem_desejada)

# Crie um gráfico de barras para a coluna "socioescola"
grafico <- ggplot(merged_df, aes(x = socioescola)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Escolaridade",
       x = "Nível",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$socioescola)/1051

### Identidade étnico-racial

# Alterar nomes em socioetnia
merged_df$socioetnia <- gsub("ama", "Amarelo", merged_df$socioetnia)
merged_df$socioetnia <- gsub("bra", "Branco", merged_df$socioetnia)
merged_df$socioetnia <- gsub("ind", "Indígena", merged_df$socioetnia)
merged_df$socioetnia <- gsub("neg", "Negro", merged_df$socioetnia)
merged_df$socioetnia <- gsub("par", "Pardo", merged_df$socioetnia)

# Tabela
table(merged_df$socioetnia)

# Crie um gráfico de barras para a coluna "socioetnia"
grafico <- ggplot(merged_df, aes(x = socioetnia)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Identidade Étnico-Racial",
       x = "Etnia",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$socioetnia)/1051

### Faixa de idade

# Alterar os nomes na coluna de renda domiciliar3
merged_df$socioidade = gsub("_", " a ", merged_df$socioidade)

# Tabela
table(merged_df$socioidade)

# Crie um gráfico de barras para a coluna "socioidade"
grafico <- ggplot(merged_df, aes(x = socioidade)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Faixa de Idade",
       x = "Faixa",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$socioidade)/1051

### Identidade de Gênero

# Remover os colchetes na coluna de pronomes utilizados
merged_df$socioidgenero = gsub("\\[|\\]", "", merged_df$socioidgenero)

# Separa os valores em diferentes linhas
library(tidyr)
dados_separados <- merged_df %>%
  separate_rows(socioidgenero, sep = ",")

# Remover os colchetes na coluna de pronomes utilizados
dados_separados$socioidgenero = gsub("\"", "", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("other", "Outro", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("agen", "Agênero", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("hom.cis", "Homem (Cis)", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("hom.trans", "Homem (Trans)", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("mul.cis", "Mulher (Cis)", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("mul.trans", "Mulher (Trans)", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("nb", "Não Binário", dados_separados$socioidgenero)
dados_separados$socioidgenero = gsub("tra", "Travesti", dados_separados$socioidgenero)

# Remover as aspas
dados_separados$socioidgenero = gsub("\"", "", dados_separados$socioidgenero)

# Tabela
table(dados_separados$socioidgenero)

# Crie um gráfico de barras para a coluna "socioidgenero"
grafico <- ggplot(dados_separados, aes(x = socioidgenero)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Identidade de Gênero",
       x = "Gênero",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(dados_separados$socioidgenero)/1051

### Estado em que nasceu

# Tabela
table(merged_df$socionasc)

# Crie um gráfico de barras para a coluna "socionasc"
grafico <- ggplot(merged_df, aes(x = socionasc)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Estado de Nascimento",
       x = "Estado",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$socionasc)/1051

### Estado em que mora

# Tabela
table(merged_df$sociomora)

# Crie um gráfico de barras para a coluna "sociomora"
grafico <- ggplot(merged_df, aes(x = sociomora)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Estado de Moradia",
       x = "Estado",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$sociomora)/1051

### Orientação Sexual

# Alterar nome em socioorient
merged_df$socioorient <- gsub("ace", "Assexual", merged_df$socioorient)
merged_df$socioorient <- gsub("bi", "Bissexual", merged_df$socioorient)
merged_df$socioorient <- gsub("het", "Heterossexual", merged_df$socioorient)
merged_df$socioorient <- gsub("homo", "Homossexual", merged_df$socioorient)
merged_df$socioorient <- gsub("other", "Outra", merged_df$socioorient)

# Tabela
table(merged_df$socioorient)

# Crie um gráfico de barras para a coluna "socioorient"
grafico <- ggplot(merged_df, aes(x = socioorient)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Orientação Sexual",
       x = "Orientação",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$socioorient)/1051

### Pronome utilizado

# Remover os colchetes na coluna de pronomes utilizados
merged_df$sociopronome = gsub("\\[|\\]", "", merged_df$sociopronome)

# Separa os valores em diferentes linhas
library(tidyr)
dados_separados <- merged_df %>%
  separate_rows(sociopronome, sep = ",")

# Remover os colchetes na coluna de pronomes utilizados
dados_separados$sociopronome = gsub("\"", "", dados_separados$sociopronome)
dados_separados$sociopronome = gsub("other", "outro", dados_separados$sociopronome)

# Tabela
table(dados_separados$sociopronome)

# Crie um gráfico de barras para a coluna "socioorient"
grafico <- ggplot(dados_separados, aes(x = sociopronome)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Pronome Pessoal",
       x = "Pronome",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(dados_separados$sociopronome)/1051

### Renda Domiciliar

# Alterar os nomes na coluna de renda domiciliar3
merged_df$sociorenda = gsub("_", " a ", merged_df$sociorenda)
merged_df$sociorenda = gsub("0.5", "Até 0,5", merged_df$sociorenda)
merged_df$sociorenda = gsub("Até 0,5 a 1", "0,5 a 1", merged_df$sociorenda)
#merged_df$sociorenda = gsub("0,5", "Até 1", merged_df$sociorenda)
merged_df$sociorenda = gsub("n", "Sem Rendimento", merged_df$sociorenda)

# Define os níveis na ordem desejada
ordem_desejada <- c("Sem Rendimento", "Até 0,5", "0,5 a 1", "1 a 2", "2 a 3", 
                    "3 a 5", "5 a 10", "10 a 20", "20+")

# Cria um fator com os níveis na ordem desejada
merged_df$sociorenda <- factor(merged_df$sociorenda, levels = ordem_desejada)

# Tabela
table(merged_df$sociorenda)

# Crie um gráfico de barras para a coluna "socioorient"
grafico <- ggplot(merged_df, aes(x = sociorenda)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição de Renda Domiciliar",
       x = "Número de Salários Mínimos",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Exiba o gráfico
print(grafico)

# Tabela dos percentuais
table(merged_df$sociorenda)/1051


### Média das respostas de cada frase ###

# Mudar inscrição nas respostas
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("neut.ani", "Machos e Fêmeas", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("neut.hum", "Homens e Mulheres", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.ani.macho", "Apenas Machos", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.ani.femea", "Apenas Fêmeas", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.hum.homem", "Apenas Homens", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.hum.mul", "Apenas Mulheres", x)))

# Tabelas dos testes
j = 1
for (i in 1:25) {
  print(table(merged_df[22 + j]))
  j = j + 2
}

# Tabelas percentuais dos testes
j = 1
for (i in 1:25) {
  print(table(merged_df[22 + j])/10.51)
  j = j + 2
}


# Histogramas com todas as possíveis respostas para cada teste

j = 1
for (i in 1:25) {
  
  # Define os níveis na ordem desejada
  ordem_desejada <- c("Apenas Fêmeas", "Apenas Machos", "Machos e Fêmeas",
                      "Apenas Mulheres", "Apenas Homens", "Homens e Mulheres")
  
  # Obtém o nome da coluna usando a variável 'i'
  nome_coluna <- colnames(merged_df)[j + 22]
  
  # Cria um fator com os níveis na ordem desejada
  merged_df[, nome_coluna] <- factor(merged_df[, nome_coluna], levels = ordem_desejada)
  
  # Crie um histograma com as cores especificadas
  grafico_histograma <- ggplot(merged_df, aes(x = !!sym(nome_coluna), fill = !!sym(nome_coluna))) +
    geom_bar() +
    labs(title = paste("Teste ", i),
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
  print(grafico_histograma)
  
  # Salve o gráfico em um arquivo PNG com o nome "meu_grafico.png" e tamanho 800x600 pixels
  ggsave(filename = paste("graficos_respostas/Hist - Teste ", i, ".png"), plot = grafico_histograma, width = 8, height = 6, dpi = 300)
  
  j = j + 2
}


### Categorias Combinadas

library(tidyr)

merged_df = banco_dados_atualizado
# Mudar inscrição nas respostas
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("neut.ani", "Machos e Fêmeas", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("neut.hum", "Homens e Mulheres", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.ani.macho", "Apenas Machos", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.ani.femea", "Apenas Fêmeas", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.hum.homem", "Apenas Homens", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.hum.mul", "Apenas Mulheres", x)))

for (i in 1:4){
  
  if(i == 1){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste01, merged_df$teste02, merged_df$teste03, sep = ",")
    num_testes = "1 a 3"
    }
  
  if(i == 2){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste04, merged_df$teste05, merged_df$teste06,
                                         merged_df$teste07, merged_df$teste08, sep = ",")
    num_testes = "4 a 8"
  }
  
  if(i == 3){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste09, merged_df$teste10, merged_df$teste11,
                                         merged_df$teste12, merged_df$teste13, merged_df$teste14,
                                         merged_df$teste15, sep = ",")
    num_testes = "9 a 15"
  }
  
  if(i == 4){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste16, merged_df$teste17, merged_df$teste18,
                                         merged_df$teste19, merged_df$teste20, merged_df$teste21,
                                         merged_df$teste22, merged_df$teste23, merged_df$teste24,
                                         merged_df$teste25, sep = ",")
    num_testes = "16 a 25"
  }
  

  # Separe os valores por vírgulas em linhas individuais
  dados_empilhados <- dados_comb %>%
    separate_rows(Coluna_Combinada, sep = ",")
  
  dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)
  
  table(dados_empilhados$Coluna_Combinada)
  table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)
  
  grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
    geom_bar() +
    labs(title = paste("Histograma de Categorias Combinadas - Testes", num_testes) ,
         x = "Categoria Combinada",
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
  ggsave(filename = paste("graficos_respostas_categoria/Categoria", i, ".png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)

}


