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




#############################             Estudo de Caso             #############################


###  Engenharia de Atributos Para E-Commerce Analytics



## Objetivo:

# - Este Estudo de Caso é uma continuação do trabalho iniciado no capítulo anterior.

# - Agora aplicaremos Engenharia de Atributos.O objetivo é analisar os dados por diferentes perspectivas e criar novas variáveis que ajudem a compreender o 
#   comportamento da variável alvo, em nosso caso se um produto será enviado com atraso ou não.

# - Todo  o  Estudo  de  Caso  é  no  contexto  de  um  problema  de  negócio  em  E-Commerce Analytics.


  
### Definição do Problema

# - Uma empresa internacional de comércio eletrônico (E-commerce) que vende produtos eletrônicos deseja descobrir informações importantes de seu banco de dados
#   de clientes.

# - Os produtos ficam armazenados em um armazém na sede da empresa. Após concluir a compra no web site da empresa, o cliente recebe o produto em casa, em qualquer 
#   parte do mundo. Os produtos são enviados de Navio, Avião ou Caminhão, dependendo da região de entrega.

# - Em cada compra o cliente pode receber um desconto dependendo do peso do produto comprado. Cada cliente pode fazer chamadas ao suporte da empresa no caso de 
#   dúvidas ou problemas e após receber o produto o cliente pode deixar uma avaliação sobre a experiência de compra. O único dado pessoal sobre o cliente que está
#   disponível é o gênero.

# - Nosso trabalho neste Estudo de Caso é explorar os dados, compreender como estão organizados, detectar eventuais problemas e analisar os dados por diferentes
#   perspectivas.

# Trabalharemos com dados fictícios que representam dados reais de uma empresa de E-Commerce. Os dados estão disponíveis na pasta "dados".




#### Carregando os Dados
df <- data.frame(read.csv2("dados/dataset.csv", sep = ","))
head(df)

## Realizando Análise Inicial (Sumário Estatístico, Veriricação de Valores NA, '' e especiais)

analise_inicial <- function(dataframe_recebido) {  # para encotrar linhas com caracter especial, vá para o fim do script
  # Sumário
  print(dim(dataframe_recebido))
  print(str(dataframe_recebido))
  print(summary(dataframe_recebido))
  cat("\n\n\n####  VERIFICANDO VALORES NA  ####\n\n")
  valores_na <- colSums(is.na(dataframe_recebido))
  if(any(valores_na > 0)) {
    cat("\n-> Colunas com valores NA:\n\n")
    print(valores_na[valores_na > 0])
  } else {
    cat("\n-> Não foram encontrados valores NA.\n")
  }
  cat("\n\n\n####  VERIFICANDO VALORES VAZIOS ''  ####\n\n")
  valores_vazios <- sapply(dataframe_recebido, function(x) sum(x == ""))
  if(any(valores_vazios > 0)) {
    cat("\n-> Colunas com valores vazios \"\":\n\n")
    print(valores_vazios[valores_vazios > 0])
  } else {
    cat("\n-> Não foram encontrados valores vazios \"\".\n")
  }
  cat("\n\n\n####  VERIFICANDO VALORES COM CARACTERES ESPECIAIS  ####\n\n")
  caracteres_especiais <- sapply(dataframe_recebido, function(x) {
    sum(sapply(x, function(y) {
      if(is.character(y) && length(y) == 1) {
        any(charToRaw(y) > 0x7E | charToRaw(y) < 0x20)
      } else {
        FALSE
      }
    }))
  })
  if(any(caracteres_especiais > 0)) {
    cat("\n-> Colunas com caracteres especiais:\n\n")
    print(caracteres_especiais[caracteres_especiais > 0])
  } else {
    cat("\n-> Não foram encontrados caracteres especiais.\n")
  }
}

analise_inicial(df)


## Modificando todas as variáveis do tipo chr para factor
df <- dplyr::mutate_if(df, is.character, as.factor)
str(df)


## Dividindo o Dataframe em Variáveis Categoricas, Variáveis Numéricas e Variável Alvo
names(df)

# Dataframe somente com as variáveis categóricas
df_categoricas <- df %>% 
  select(where(is.factor))

# Dataframe somente com as variáveis numéricas
df_numericas <- df %>% 
  select(where(is.numeric)) %>% 
  select(-ID, -entregue_no_prazo)

# Dataframe somente com a variável alvo/target "entregue_no_prazo"
df_target <- df %>%
  select(entregue_no_prazo) %>% 
  mutate(entregue_no_prazo = factor(entregue_no_prazo))




#### Limpeza De Dados


## Tratamento de Valores ausentes

# - Atenção -> Valor ausente significa ausência de informação e não ausência de dado!

# - O tratamento pode ser feito antes, durante ou depois da Análise Exploratória, mas idealmente deve ser feito antes da Engenharia de Atributos.
#   Mas fique atento: a Engenharia de Atributos e o Pré-Processamento podem gerar valores ausentes, o que precisa ser tratado.

analise_inicial(df)

colSums(is.na(df))

# -> Não foi constatado dados ausentes.



## Tratamento de Valores Duplicados

