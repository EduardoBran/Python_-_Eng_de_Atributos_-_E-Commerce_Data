####  Big Data Real-Time Analytics com Python e Spark  ####

# Configurando o diretório de trabalho
setwd("~/Desktop/DataScience/CienciaDeDados/2.Big-Data-Real-Time-Analytics-com-Python-e-Spark/5.Engenharia_de_Atributos")
getwd()



## Importando Pacotes
library(readxl)         # carregar arquivos
library(dplyr)          # manipula dados
library(tidyr)          # manipula dados (funcao pivot_longer)
library(ggplot2)        # gera gráficos
library(patchwork)      # unir gráficos
library(corrplot)       # mapa de Correlação

library(randomForest)





#############################             Lista de Exercícios             #############################





### Criando Massa de Dados Para os Exercícios

# DataFrame 1
df_a <- data.frame(list(
  id_disciplina = c('1', '2', '3', '4', '5'),
  nome = c('Bob', 'Maria', 'Mateus', 'Ivo', 'Gerson'),
  sobrenome = c('Anderson', 'Teixeira', 'Amoedo', 'Trindade', 'Vargas')
))
df_a

# DataFrame 2
df_b <- data.frame(list(
  id_disciplina = c('4', '5', '6', '7', '8'),
  nome = c('Roberto', 'Mariana', 'Ana', 'Marcos', 'Maria'),
  sobrenome = c('Sampaio', 'Fernandes', 'Arantes', 'Menezes', 'Martins')
))
df_b

# DataFrame 3
df_c <- data.frame(list(
  id_disciplina = c('1', '2', '3', '4', '5', '7', '8', '9', '10', '11'),
  id_teste = c(81, 75, 75, 71, 76, 84, 95, 61, 57, 90)
))
df_c


## Exercício 1

# - Faça a concatenação dos dataframes a e b pelas linhas e crie um novo dataframe chamado df_ab_linha.

# Concatenar os DataFrames pelas linhas
df_ab_linha <- bind_rows(df_a, df_b)
df_ab_linha





## Exercício 2

# - Faça a concatenação dos dataframes a e b pelas colunas e crie um novo dataframe chamado df_ab_coluna.

# Concatenar os DataFrames pelas colunas
df_ab_coluna <- bind_cols(df_a, df_b)
df_ab_coluna




## Exercício 3

# - Faça o merge dos dataframes df_ab_linha e df_c usando a coluna id_disciplina

df_merge_ex3 <- inner_join(df_ab_linha, df_c, by = 'id_disciplina')
df_merge_ex3





## Exercício 4

# - Faça o merge dos dataframes df_ab_linha e df_c pela coluna id_disciplina usando left e right

merge_left <- left_join(df_ab_linha, df_c, by = 'id_disciplina')
merge_right <- right_join(df_ab_linha, df_c, by = 'id_disciplina')

merge_left
merge_right





## Exercício 5

# - Faça o outer join entre os dataframes df_a e df_b usando a coluna id_disciplina
#   (Merge outer join é a união externa completa que produz o conjunto de todos os registros na Tabela A e na Tabela B, com registros correspondentes
#    de ambos os lados, quando disponíveis. Se não houver correspondência, o lado ausente conterá null.)

merge_outer <- full_join(df_a, df_b, by = "id_disciplina")
merge_outer




## Exercício 6

# - O exercício 5 gera valores ausentes. Não deixe isso acontecer ao fazer o merge!





## Exercício 7

# - Faça o merge inner join entre os dataframes df_a e df_a usando a coluna id_disciplina
#   (Merge inner join é a junção interna que produz apenas o conjunto de registros que correspondem na Tabela A e na Tabela B.)





## Exercício 8

# - Faça o merge left join entre os dataframes df_a e df_a usando a coluna id_disciplina
#   (Merge left join é a junção externa esquerda que produz um conjunto completo de registros da Tabela A, com os registros correspondentes,
#    quando disponíveis, na Tabela B. Se não houver correspondência, o lado direito conterá nulo.)




## Exercício 9

# - O exercício 8 gera valores ausentes. Não deixe isso acontecer ao fazer o merge!





## Exercício 10

# - Faça o merge right join entre os dataframes df_a e df_a usando a coluna id_disciplina
#   (O merge right join é contrário do left join.)

#   Não deixar valores ausentes!





## Exercício 11

# - Você percebeu que o item anterior gerou nomes de colunas muito parecidos.
# - Adicione um sufixo para identificar os nomes das colunas.
#   Não deixar valores ausentes!





## Exercício 12

# - Faça o merge dos dataframes df_a e df_b com base no índice.




