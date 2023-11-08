### Monografia Lucas - Script Principal ###

library(readr)
library(ggplot2)
install.packages("writexl")
library(writexl)

dados_pavlovia <- read_csv("Dados (final) - cidades ajustadas.csv")
View(dados_pavlovia)

#dados_brutos <- read.csv("Dados (bruto).csv")


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
#dados = subset(dados, socioescolasuperior != cursos_para_excluir)

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

# Ler biblioteca dplyr
library(dplyr)

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

# Encontre as linhas com elementos repetidos na coluna "Coluna1"
#repeticoes <- municipios_brasil[duplicated(municipios_brasil$sociomora) | duplicated(municipios_brasil$sociomora, fromLast = TRUE), ]

# Exiba as linhas com repetições
#print(repeticoes)

# Use inner_join para encontrar elementos comuns entre os dataframes
#elementos_comuns <- inner_join(dados, repeticoes, by = c("sociomora" = "sociomora"))

# Exiba os elementos comuns encontrados
#print(elementos_comuns)

# Encontre as linhas onde "termo_procurado" aparece na coluna "Coluna1"
#linhas_com_termo <- municipios_brasil[municipios_brasil$sociomora == "rio branco", ]

# Exiba as linhas encontradas
#print(linhas_com_termo)

# Encontre as linhas onde "termo_procurado" aparece na coluna "Coluna1"
#linhas_com_termos <- dados[municipios_brasil$sociomora == "rio branco" & municipios_brasil$Estado == "AC", ]


# Realize um join baseado na coluna 'Cidade'
merged_df <- inner_join(dados[-c(2,4,5,76:85)], municipios_brasil, by = "sociomora")

# Encontrar cidades com nomes repetidos em estados diferentes
repeticoes_tempo_resposta = merged_df[duplicated(merged_df$consentimento_rt) | duplicated(merged_df$consentimento_rt, fromLast = TRUE), ]

# Encontre os valores únicos dentre as cidades com nomes repetidos em estados diferentes
valores_unicos <- unique(repeticoes_tempo_resposta$sociomora)

# Exiba os valores únicos
#print(valores_unicos)

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


library(dplyr)

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

# Instale e carregue as bibliotecas necessárias
install.packages("knitr")
install.packages("kableExtra")
library(knitr)
library(kableExtra)

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

# Crie um dataframe de exemplo
teste <- data.frame(
  Nome = c("João", "Maria", "Pedro"),
  Idade = c(30, 25, 35),
  Pontuação = c(80, 90, 75)
)

# Crie uma tabela formatada com kable() e personalize-a com kableExtra()
tabela_formatada <- kable(teste, "html") %>%
  kable_styling(full_width = FALSE) %>%
  column_spec(1, bold = TRUE) %>%
  column_spec(2:3, color = "black") %>%
  row_spec(0, bold = TRUE, color = "black")

# Salve a tabela como uma imagem
#save_kable(tabela_formatada, "tabela.png")

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

library(ggplot2)

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
  ggsave(filename = paste("Hist - Teste ", i, ".png"), plot = grafico_histograma, width = 8, height = 6, dpi = 300)
  
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


dados_comb = merged_df
dados_comb$Coluna_Combinada <- paste(merged_df$teste01, merged_df$teste02, merged_df$teste03, sep = ",")

dados_comb = merged_df
dados_comb$Coluna_Combinada <- paste(merged_df$teste04, merged_df$teste05, merged_df$teste06,
                                     merged_df$teste07, merged_df$teste08, sep = ",")

dados_comb = merged_df
dados_comb$Coluna_Combinada <- paste(merged_df$teste09, merged_df$teste10, merged_df$teste11,
                                     merged_df$teste12, merged_df$teste13, merged_df$teste14,
                                     merged_df$teste15, sep = ",")

dados_comb = merged_df
dados_comb$Coluna_Combinada <- paste(merged_df$teste16, merged_df$teste17, merged_df$teste18,
                                     merged_df$teste19, merged_df$teste20, merged_df$teste21,
                                     merged_df$teste22, merged_df$teste23, merged_df$teste24,
                                     merged_df$teste25, sep = ",")

# Separe os valores por vírgulas em linhas individuais
dados_empilhados <- dados_comb %>%
  separate_rows(Coluna_Combinada, sep = ",")

dados_empilhados$Coluna_Combinada <- factor(dados_empilhados$Coluna_Combinada, levels = ordem_desejada)

table(dados_empilhados$Coluna_Combinada)
table(dados_empilhados$Coluna_Combinada)/nrow(dados_empilhados)

grafico_histograma_comb <- ggplot(dados_empilhados, aes(x = Coluna_Combinada, fill = Coluna_Combinada)) +
  geom_bar() +
  labs(title = "Histograma de Categorias Combinadas - Testes 16 a 25",
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
ggsave(filename = paste("graficos_respostas_categoria/Categoria 4.png"), plot = grafico_histograma_comb, width = 8, height = 6, dpi = 300)



### Tempos de resposta

# Estatísticas dos testes

merged_df = banco_dados_atualizado

# Crie vetores para armazenar os resultados
medias <- numeric(25)
variancias <- numeric(25)
desvios_padrao <- numeric(25)
minimos <- numeric(25)
maximos <- numeric(25)
soma <- numeric(25)

# Use um loop para calcular as estatísticas para cada coluna
j = 1
for (i in 1:25) {
  coluna <- merged_df[[23+j]]  # Acesse a coluna pelo índice
  medias[i] <- mean(coluna)
  variancias[i] <- var(coluna)
  desvios_padrao[i] <- sd(coluna)
  minimos[i] <- min(coluna)
  maximos[i] <- max(coluna)
  soma[i] <- sum(coluna)
  j = j + 2
}

# Exiba os resultados
resultados <- data.frame(
  Teste = 1:25,
  Media = medias,
  Variancia = variancias,
  Desvio_Padrao = desvios_padrao,
  Minimo = minimos,
  Maximo = maximos,
  Soma = soma
)

write_xlsx(resultados, path = "Estatísticas - rt.xlsx")
write.csv(resultados, file = "Estatísticas - rt.csv", row.names = FALSE)


# Distribuição dos testes

# Crie um loop para gerar os gráficos de distribuição
j = 1
for (i in 1:25) {
  # Transforma a coluna em dados numéricos
  merged_df[[j + 23]] = as.numeric(merged_df[[j + 23]])
  
  # Calcule os quartis
  Q1 <- quantile(merged_df[[j + 23]], 0.25)
  Q3 <- quantile(merged_df[[j + 23]], 0.75)
  
  # Calcule o intervalo interquartil (IQR)
  IQR <- Q3 - Q1
  
  # Defina um limite inferior e superior para os outliers
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  
  # Remova os outliers da coluna
  dados_sem_outliers <- merged_df[merged_df[[j + 23]] >= limite_inferior & merged_df[[j + 23]] <= limite_superior, ]
  
  # Realize o teste de normalidade (Shapiro-Wilk)
  resultado_teste_normalidade <- shapiro.test(dados_sem_outliers[[j + 23]])
  
  # Distribuição dos dados sem outliers usando ggplot2
  grafico_histograma <- ggplot(dados_sem_outliers, aes(x = dados_sem_outliers[[j + 23]])) +
    geom_histogram(binwidth = 1) +  # Personalize o tamanho dos bins conforme necessário
    labs(title = paste("Distribuição de RT - Teste", i),
         x = "Valores",
         y = "Frequência") +
    geom_density(aes(y = ..count..), color = "blue") +  # Adicione a curva da distribuição normal
    annotate("text", x = max(dados_sem_outliers[[j + 23]]) - 5, y = max(dados_sem_outliers$count) - 5,
             label = ifelse(resultado_teste_normalidade$p.value < 0.05, "NÃO Normal", "Normal"),
             color = "red", size = 5)  # Indique o resultado do teste de normalidade no gráfico
  
  # Salve o gráfico como um arquivo PNG (ou outro formato desejado)
  nome_arquivo <- paste0("graficos_histograma/histograma_teste_", i, ".png")
  ggsave(nome_arquivo, plot = grafico_histograma, width = 6, height = 4)  # Ajuste o tamanho conforme necessário
  
  j = j + 2
}


# Estatísticas das 4 categorias

merged_df = banco_dados_atualizado

# Mudar inscrição nas respostas
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("neut.ani", "Machos e Fêmeas", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("neut.hum", "Homens e Mulheres", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.ani.macho", "Apenas Machos", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.ani.femea", "Apenas Fêmeas", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.hum.homem", "Apenas Homens", x)))
merged_df <- as.data.frame(lapply(merged_df, function(x) gsub("exc.hum.mul", "Apenas Mulheres", x)))


# Crie vetores para armazenar os resultados
medias <- numeric(4)
variancias <- numeric(4)
desvios_padrao <- numeric(4)
minimos <- numeric(4)
maximos <- numeric(4)
soma <- numeric(4)

for (i in 1:4){
  
  if(i==1){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste01_rt, merged_df$teste02_rt, merged_df$teste03_rt, sep = ",")
  }
  
  if(i==2){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste04_rt, merged_df$teste05_rt, merged_df$teste06_rt,
                                     merged_df$teste07_rt, merged_df$teste08_rt, sep = ",")
  }
  
  if(i==3){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste09_rt, merged_df$teste10_rt, merged_df$teste11_rt,
                                     merged_df$teste12_rt, merged_df$teste13_rt, merged_df$teste14_rt,
                                     merged_df$teste15_rt, sep = ",")
  }
  
  if(i==4){
    dados_comb = merged_df
    dados_comb$Coluna_Combinada <- paste(merged_df$teste16_rt, merged_df$teste17_rt, merged_df$teste18_rt,
                                     merged_df$teste19_rt, merged_df$teste20_rt, merged_df$teste21_rt,
                                     merged_df$teste22_rt, merged_df$teste23_rt, merged_df$teste24_rt,
                                     merged_df$teste25_rt, sep = ",")
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
  
# Exiba os resultados por categoria
  resultados_categoria <- data.frame(
    Categoria = 1:4,
    Media = medias,
    Variancia = variancias,
    Desvio_Padrao = desvios_padrao,
    Minimo = minimos,
    Maximo = maximos,
    Soma = soma
  )
  
}

write_xlsx(resultados_categoria, path = "Estatísticas por Categoria - rt.xlsx")
write.csv(resultados_categoria, file = "Estatísticas por Categoria - rt.csv", row.names = FALSE)


### Testes de Hipóteses / ANOVA para o tempo de resposta das categorias

# Suponha que você tenha as médias, variâncias/desvios padrão e o número de observações
# em vetores ou variáveis separadas, como 'media', 'variancia', 'num_observacoes'

# Defina a média da Categoria 3
media_categoria_3 <- resultados_categoria$Media[3]

# Crie um vetor com as médias das outras categorias
outras_medias <- c(resultados_categoria$Media[1], resultados_categoria$Media[2], resultados_categoria$Media[4])

# Crie um vetor com as variâncias ou desvios padrão das outras categorias
outras_variancias <- c(resultados_categoria$Variancia[1], resultados_categoria$Variancia[2], resultados_categoria$Variancia[4])

# Crie um vetor com o número de observações das outras categorias
outras_observacoes <- c(3*nrow(merged_df), 5*nrow(merged_df), 10*nrow(merged_df))

# Realize um teste t de duas amostras para comparar a Categoria 3 com cada outra categoria
for (i in 1:length(outras_medias)) {
  # Realize o teste t de duas amostras
  resultado_teste <- t.test(x = c(media_categoria_3, outras_medias[i]), 
                            y = c(sqrt(resultados_categoria$Variancia[3])/sqrt(7*nrow(merged_df)), 
                                  sqrt(outras_variancias[i])/sqrt(outras_observacoes[i])),
                            alternative = "greater")
  
  # Exiba o resultado do teste para cada comparação
  if(i==3){
    i = i + 1
  }
  print(paste("Comparação com a Categoria", i))
  print(resultado_teste)
}



###

# Defina a média da Categoria 3
media_categoria_2 <- resultados_categoria$Media[2]

# Crie um vetor com as médias das outras categorias
outras_medias <- c(resultados_categoria$Media[1], resultados_categoria$Media[4])

# Crie um vetor com as variâncias ou desvios padrão das outras categorias
outras_variancias <- c(resultados_categoria$Variancia[1], resultados_categoria$Variancia[4])

# Crie um vetor com o número de observações das outras categorias
outras_observacoes <- c(3*nrow(merged_df), 10*nrow(merged_df))

# Realize um teste t de duas amostras para comparar a Categoria 3 com cada outra categoria
j = 1
for (i in 1:length(outras_medias)) {
  # Realize o teste t de duas amostras
  resultado_teste <- t.test(x = c(media_categoria_2, outras_medias[i]), 
                            y = c(sqrt(resultados_categoria$Variancia[2])/sqrt(5*nrow(merged_df)), 
                                  sqrt(outras_variancias[i])/sqrt(outras_observacoes[i])),
                            alternative = "greater")
  
  # Exiba o resultado do teste para cada comparação
  print(paste("Comparação com a Categoria", j))
  print(resultado_teste)
  j = 4
}


###

# Realize o teste t de duas amostras
resultado_teste <- t.test(x = c(resultados_categoria$Media[1], resultados_categoria$Media[4]), 
                          y = c(sqrt(resultados_categoria$Variancia[1])/sqrt(3*nrow(merged_df)), 
                                sqrt(resultados_categoria$Variancia[4])/sqrt(10*nrow(merged_df))),
                          alternative = "greater")

# Exiba o resultado do teste para cada comparação
print(paste("Comparação com a Categoria", 4))
print(resultado_teste)



print((resultados_categoria$Media[1] - resultados_categoria$Media[2])/(resultados_categoria$Desvio_Padrao[1]/sqrt(3*nrow(merged_df)) - resultados_categoria$Desvio_Padrao[2]/sqrt(5*nrow(merged_df))))

print((resultados_categoria$Media[1] - resultados_categoria$Media[3])/(resultados_categoria$Desvio_Padrao[1]/sqrt(3*nrow(merged_df)) - resultados_categoria$Desvio_Padrao[3]/sqrt(7*nrow(merged_df))))

print((resultados_categoria$Media[1] - resultados_categoria$Media[4])/(resultados_categoria$Desvio_Padrao[1]/sqrt(3*nrow(merged_df)) - resultados_categoria$Desvio_Padrao[4]/sqrt(10*nrow(merged_df))))

print((resultados_categoria$Media[2] - resultados_categoria$Media[3])/(resultados_categoria$Desvio_Padrao[2]/sqrt(5*nrow(merged_df)) - resultados_categoria$Desvio_Padrao[3]/sqrt(7*nrow(merged_df))))

print((resultados_categoria$Media[2] - resultados_categoria$Media[4])/(resultados_categoria$Desvio_Padrao[2]/sqrt(5*nrow(merged_df)) - resultados_categoria$Desvio_Padrao[4]/sqrt(10*nrow(merged_df))))

print((resultados_categoria$Media[3] - resultados_categoria$Media[4])/(resultados_categoria$Desvio_Padrao[3]/sqrt(7*nrow(merged_df)) - resultados_categoria$Desvio_Padrao[4]/sqrt(10*nrow(merged_df))))



























# Gráficos de pizza com todas as possíveis respostas para cada teste

library(ggplot2)

j = 1
for (i in 1:25) {

# Crie uma tabela de frequência das respostas
tabela_frequencia <- table(merged_df[22 +j])
j = j + 2

# Crie um dataframe a partir da tabela de frequência
df_frequencia <- as.data.frame(tabela_frequencia)

# Renomeie as colunas para nomes mais descritivos
colnames(df_frequencia) <- c("Resposta", "Contagem")

# Crie o gráfico de pizza
grafico_pizza <- ggplot(df_frequencia, aes(x = "", y = Contagem, fill = Resposta)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +  # Transforma o gráfico em um gráfico de pizza
  labs(title = "Distribuição de Respostas",
       fill = "Resposta") +
  scale_fill_manual(values = c("Apenas Fêmeas" = "pink", "Apenas Machos" = "blue", "Machos e Fêmeas" = "purple", "Apenas Mulheres" = "red", "Apenas Homens" = "green", "Homens e Mulheres" = "yellow")) +  # Defina cores personalizadas para as respostas
  theme_minimal() +
  theme(legend.position = "right")

# Exiba o gráfico de pizza
print(grafico_pizza)

}





# load library
library(ggplot2)

teste_1 = merged_df[,c(1,23)]

# Compute percentages
teste_1$fraction = teste_1$count / sum(teste_1$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart





# Barras empilhadas para comparar dentro dos grupos
















