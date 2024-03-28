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




#############################             Estudo de Caso             #############################


####  Engenharia de Atributos Para E-Commerce Analytics



### Objetivo:

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
  cat("\n\n####  DIMENSÕES  ####\n\n")
  print(dim(dataframe_recebido))
  cat("\n\n\n####  INFO  ####\n\n")
  print(str(dataframe_recebido))
  cat("\n\n\n####  SUMÁRIO  ####\n\n")
  print(summary(dataframe_recebido))
  cat("\n\n\n####  VERIFICANDO QTD DE LINHAS DUPLICADAS  ####\n\n")
  print(sum(duplicated(dataframe_recebido)))
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




##################################    Limpeza De Dados    ################################## 


#### Tratamento de Valores ausentes

##  -> Atenção: Valor ausente significa ausência de informação e não ausência de dado!
# - O tratamento pode ser feito antes, durante ou depois da Análise Exploratória, mas idealmente deve ser feito antes da Engenharia de Atributos.
#   Mas fique atento: a Engenharia de Atributos e o Pré-Processamento podem gerar valores ausentes, o que precisa ser tratado.

analise_inicial(df)

colSums(is.na(df))

# -> Não foi constatado dados ausentes.





#### Tratamento de Valores Duplicados

##  -> Atenção: Valores duplicados significam duplicidade dos dados em toda a linha (todo o registro).
# - O tratamento pode ser feito antes, durante ou depois da Análise Exploratória, mas idealmente deve ser feito antes da Engenharia de Atributos.

sum(duplicated(df))





#### Tratamento de Valores Outliers


## Tratando Uma Variável Específica "desconto" (depois tratamos todas de uma vez)

# Verificando Média e Desvio Padrão
df_desconto <- df %>% 
  select(desconto) %>% 
  summarise(Media = mean(desconto),
            Desvio_Padrao = sd(desconto))
df_desconto

# Histograma
hist(df$desconto)

# Calcula os limites superior e inferior
# (Um valor outlier é aquele que está abaixo do limite inferior ou acima do limite superior)

limite_superior = df_desconto$Media + 3 * df_desconto$Desvio_Padrao
cat("Limite superior:", limite_superior)
limite_inferior = df_desconto$Media - 3 * df_desconto$Desvio_Padrao
cat("Limite inferior:", limite_inferior)

# Filtra o dataframe removendo os registros com outliers na coluna desconto
dim(df)
df <- df %>%
  filter(desconto > limite_inferior & desconto < limite_superior)
dim(df)


## Tratando Todas as Variáveis Numéricas (exceto desconto)

# Nomes das colunas numéricas de interesse
nums2 <- c("numero_chamadas_cliente", 
           "avaliacao_cliente", 
           "compras_anteriores", 
           "custo_produto", 
           "peso_gramas")

# Calculando z-scores e removendo outliers
df_filtrado <- df %>%
  # Calcula o z-score para cada coluna numérica
  mutate(across(all_of(nums2), list(z = ~ (.-mean(.))/sd(.)), .names = "{.col}_zscore")) %>%         # " across(where(is.numeric) " - (todas var numéricas)
  # Filtra linhas removendo outliers (qualquer z-score fora de [-3, 3])
  filter(if_all(ends_with("zscore"), ~ abs(.) < 3)) %>%
  # Remove as colunas de z-score adicionadas anteriormente
  select(-ends_with("zscore"))

# Comparando as dimensões antes e depois do filtro
cat("Dimensões do DataFrame antes do filtro: ", dim(df), "\n")
cat("Dimensões do DataFrame após filtrar outliers: ", dim(df_filtrado), "\n")

df <- df_filtrado

rm(limite_inferior, limite_superior, nums2, df_filtrado)





#### Feature Selection

##  -> Atenção: Aqui tomamos as decisões sobre quais variáveis serão usadas na Engenharia de Atributos.


# Removendo variável ID
df_sem_ID <- df[, !names(df) %in% c("ID")]


## Utilizando modelo randomForest para o método de Feature Selection

modelo <- randomForest(entregue_no_prazo ~ ., 
                       data = df_sem_ID, 
                       ntree = 100, nodesize = 10, importance = T)

# Visualizando por números
print(modelo$importance)

# Visualizando por Gráficos
varImpPlot(modelo)

importancia_ordenada <- modelo$importance[order(-modelo$importance[, 1]), , drop = FALSE] 
df_importancia <- data.frame(
  Variavel = rownames(importancia_ordenada),
  Importancia = importancia_ordenada[, 1]
)
ggplot(df_importancia, aes(x = reorder(Variavel, -Importancia), y = Importancia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Importância das Variáveis", x = "Variável", y = "Importância") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10))

rm(modelo, importancia_ordenada, df_importancia, df_sem_ID)


# -> Com base nos gráficos seriam eliminadasas variáveis modo_envio, genero, prioridade_produto e avaliacao_cliente


### Aplicando Feature Selection separadamente nas Variáveis NUméricas (correlação) e Variáveis Categóricas (gráficos)


## Variáveis Numéricas (correlação)
df_cor <- df %>% 
  select(-ID, -corredor_armazem, -modo_envio, -prioridade_produto, -genero)

corrplot(cor(df_cor),
         method = "color",
         type = "upper",
         addCoef.col = 'springgreen2',
         tl.col = "black",
         tl.srt = 45)
rm(df_cor)

# -> Com base no critério de de que as variáveis que ultrapassaram o limite (+/- 0,05) na correlação com varíavel alvo/target (entregue_no_prazo) serão
#    escolhidos para serem processados na Engenharia de Atributos, nesse caso foram as escolhidas as variáveis:
#    numero_chamadas_cliente, custo_produto, compras_anteriores, desconto e peso_gramas


## Variáveis Categóricas (gráficos de barras)

# Gerando Gráficos
df_long <- df_categoricas %>%
  pivot_longer(cols = everything(), names_to = "Variavel", values_to = "Categoria")

ggplot(df_long, aes(x = Categoria, fill = Categoria)) +
  geom_bar() +
  facet_wrap(~ Variavel, scales = "free_x", nrow = 2) +
  labs(title = "Distribuição das Variáveis Categóricas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.position = "none",
        strip.background = element_blank(),
        strip.text.x = element_text(size = 12)) +
  xlab("Categoria") +
  ylab("Contagem") +
  guides(fill=guide_legend(title="Categoria")) # Adiciona a legenda de volta, se necessário

rm(df_long)

# -> Com base nos gráficos percebe-se que a variável genêro está praticamente balancedada, desta forma ela não impactaria no resultado final.


# -> Para ficar igual ao curso seriam ELIMINADAS as variáveis "avaliacao_cliente" e "genero"





### Obs: Como as tarefas a seguir são complexas, demonstraremos apenas como algumas colunas. Fique à vontade para refazer a Engenharia de Atributos 
###      usando todas as colunas selecionadas conforme regras definidas acima.





#### Feature Extraction (criando novas variáveis)

## -> ATENÇÃO: Aqui fazemos a extração de novas variáveis a partir da informação contida em outras variáveis.


# Criando uma cópia
df_eng <- df %>% select(-ID)
str(df_eng)
names(df_eng)



## Utilizando as variáveis prioridade_produto e entregue_no_prazo

# Criando uma Nova Variável com a Performance de Envio do Produto Por Prioridade do Produto

# -> Todo atraso no envio dos produtos é igual, ou seja, tem a mesma proporção? A prioridade de envio dos produtos gera mais ou menos atrasos?
# -> Criaremos uma nova variável que representa a performance do envio do produto com base na seguinte regra de negócio (levels):
  
# Se a prioridade do produto era alta e houve atraso no envio, o atraso é crítico.
# Se a prioridade do produto era média e houve atraso no envio, o atraso é problemático.
# Se a prioridade do produto era baixa e houve atraso no envio, o atraso é tolerável.
# Outra opção significa que o envio foi feito no prazo e não apresenta problema.


# Criando nova coluna "performance_prioridade_envio" e preenchendo com valores NA
df_eng$performance_prioridade_envio <- NA

# Alimentando nova coluna
df_eng$performance_prioridade_envio <- ifelse(
  df_eng$prioridade_produto == 'alta' & df_eng$entregue_no_prazo == 0, "Atraso Crítico",
  ifelse(
    df_eng$prioridade_produto == 'media' & df_eng$entregue_no_prazo == 0, "Atraso Problemático",
    ifelse(
      df_eng$prioridade_produto == 'baixa' & df_eng$entregue_no_prazo == 0, "Atraso Tolerável",
      "Não Houve Atraso"
    )
  )
)
df_eng$performance_prioridade_envio <- as.factor(df_eng$performance_prioridade_envio)

# Visualizando
summary(df_eng$performance_prioridade_envio)



## Criando um novo DataFrame de Análise para a nova variável performance_prioridade_envio

# Gerando um dataframe com as análises
df_report1 <- df_eng %>% 
  group_by(performance_prioridade_envio, entregue_no_prazo) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  as.data.frame()
df_report1

# Aplicando Pivot ("girando os dados", transformando linhas em colunas e colunas em linhas)
df_report1 <- df_report1 %>%
  pivot_wider(names_from = entregue_no_prazo, 
              values_from = contagem) %>% 
  as.data.frame()
df_report1

# Renomenado Colunas
names(df_report1) <- c('Status_do_Envio', 'Total_Atraso', 'Total_no_Prazo')
df_report1

# Substituindo NA por zero
df_report1 <- df_report1 %>%
  mutate(Total_Atraso = if_else(is.na(Total_Atraso), 0, Total_Atraso),
         Total_no_Prazo = if_else(is.na(Total_no_Prazo), 0, Total_no_Prazo))

# Concatenando as colunas "Total Atraso" e "Total no Prazo" para criar uma nova coluna "Total" e remove as colunas "Total Atraso" e "Total no Prazo"
df_report1 <- df_report1 %>%
  mutate(Total = Total_Atraso + Total_no_Prazo) %>% 
  select(-Total_Atraso, -Total_no_Prazo) %>% 
  as.data.frame()
df_report1
str(df_report1)

# Gráfico de Barras
ggplot(df_report1, aes(x = Status_do_Envio, y = Total, fill = Status_do_Envio)) +
  geom_col() +  # Usamos geom_col() para valores pré-tabulados
  labs(x = "Status do Envio", y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")  # Usando uma paleta de cores para valores discretos




## Utilizando as variáveis modo_envio, prioridade_produto e entregue_no_prazo

# Criando uma Nova Variável com a Performance de Envio do Produto Por Modo de Envio e Prioridade do Produto

#  -> O modo de envio dos produtos associado à proridade de envio dos produtos, tem impacto no atraso dos produtos?
#  -> Criaremos uma nova variável que representa a performance da variável alvo **entregue_no_prazo** por **modo_envio** e **prioridade_poduto** com base 
#     nas seguintes regras de negócio:

# Se a prioridade do produto era alta, o modo de envio era Navio e houve atraso no envio, o atraso é crítico por Navio.
# Se a prioridade do produto era média, o modo de envio era Navio e houve atraso no envio, o atraso é problemático por Navio.
# Se a prioridade do produto era baixa, o modo de envio era Navio e houve atraso no envio, o atraso é tolerável por Navio.
# Se a prioridade do produto era alta, o modo de envio era Aviao e houve atraso no envio, o atraso é crítico por Aviao.
# Se a prioridade do produto era média, o modo de envio era Aviao e houve atraso no envio, o atraso é problemático por Aviao.
# Se a prioridade do produto era baixa, o modo de envio era Aviao e houve atraso no envio, o atraso é tolerável por Aviao.
# Se a prioridade do produto era alta, o modo de envio era Caminhao e houve atraso no envio, o atraso é crítico por Caminhao.
# Se a prioridade do produto era média, o modo de envio era Caminhao e houve atraso no envio, o atraso é problemático por Caminhao.
# Se a prioridade do produto era baixa, o modo de envio era Caminhao e houve atraso no envio, o atraso é tolerável por Caminhao.
# Outra opção significa que o envio foi feito no prazo e não apresenta problema.


# Criando nova coluna "performance_modo_envio" e preenchendo com valores NA
df_eng$performance_modo_envio <- NA

# Alimentando nova coluna
df_eng <- df_eng %>%
  mutate(performance_modo_envio = case_when(
    prioridade_produto == 'alta' & modo_envio == 'Navio' & entregue_no_prazo == 0 ~ "Atraso Crítico na Entrega Por Navio",
    prioridade_produto == 'media' & modo_envio == 'Navio' & entregue_no_prazo == 0 ~ "Atraso Problemático na Entrega Por Navio",
    prioridade_produto == 'baixa' & modo_envio == 'Navio' & entregue_no_prazo == 0 ~ "Atraso Tolerável na Entrega Por Navio",
    prioridade_produto == 'alta' & modo_envio == 'Aviao' & entregue_no_prazo == 0 ~ "Atraso Crítico na Entrega Por Aviao",
    prioridade_produto == 'media' & modo_envio == 'Aviao' & entregue_no_prazo == 0 ~ "Atraso Problemático na Entrega Por Aviao",
    prioridade_produto == 'baixa' & modo_envio == 'Aviao' & entregue_no_prazo == 0 ~ "Atraso Tolerável na Entrega Por Aviao",
    prioridade_produto == 'alta' & modo_envio == 'Caminhao' & entregue_no_prazo == 0 ~ "Atraso Crítico na Entrega Por Caminhao",
    prioridade_produto == 'media' & modo_envio == 'Caminhao' & entregue_no_prazo == 0 ~ "Atraso Problemático na Entrega Por Caminhao",
    prioridade_produto == 'baixa' & modo_envio == 'Caminhao' & entregue_no_prazo == 0 ~ "Atraso Tolerável na Entrega Por Caminhao",
    TRUE ~ "Não Houve Atraso"
  ))
df_eng$performance_modo_envio <- as.factor(df_eng$performance_modo_envio)

# Visualizando
summary(df_eng$performance_modo_envio)


## Criando um novo DataFrame de Análise para a nova variável performance_modo_envio

# Gerando um dataframe com as análises
df_report2 <- df_eng %>% 
  group_by(performance_modo_envio, entregue_no_prazo) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  as.data.frame()
df_report2

# Aplicando Pivot ("girando os dados", transformando linhas em colunas e colunas em linhas)
df_report2 <- df_report2 %>%
  pivot_wider(names_from = entregue_no_prazo, 
              values_from = contagem) %>% 
  as.data.frame()
df_report2

# Renomenado Colunas
names(df_report2) <- c('Status_do_Envio', 'Total_Atraso', 'Total_no_Prazo')
df_report2

# Substituindo NA por zero
df_report2 <- df_report2 %>%
  mutate(Total_Atraso = if_else(is.na(Total_Atraso), 0, Total_Atraso),
         Total_no_Prazo = if_else(is.na(Total_no_Prazo), 0, Total_no_Prazo))

# Concatenando as colunas "Total Atraso" e "Total no Prazo" para criar uma nova coluna "Total" e remove as colunas "Total Atraso" e "Total no Prazo"
df_report2 <- df_report2 %>%
  mutate(Total = Total_Atraso + Total_no_Prazo) %>% 
  select(-Total_Atraso, -Total_no_Prazo) %>% 
  as.data.frame()
df_report2
str(df_report2)

# Gráfico de Barras
ggplot(df_report2, aes(x = Status_do_Envio, y = Total, fill = Status_do_Envio)) +
  geom_col() +
  labs(x = "Status do Envio", y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = rep(c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000"),
                                 length.out = length(unique(df_report2$Status_do_Envio))))



## Utilizando as variáveis desconto e entregue_no_prazo

# Criando Novas Variáveis com a Performance de Envio dos Produtos Considerando os Descontos

#  -> Há diferença na performance de envio dos produtos quando o produto recebe algum tipo de desconto?
#  -> Criaremos duas novas variáveis.

#  -> A primeira variávei será faixa_desconto e será criada usando dados da variável desconto.
#  -> A segunda variável será performance_faixa_desconto e será criada usando dados da nova variável faixa_desocnto e entregue_no_prazo
#  -> Criaremos as duas novas variáveis com base na seguinte regra de negócio:
  
##  Variável 1 - faixa_desconto

# Desconto acima ou igual à média
# Desconto abaixo da média

## Variável 2 - performance_faixa_desconto

# Se a faixa de desconto foi acima ou igual à média e houve atraso na entrega = "Atraso na Entrega com Desconto Acima da Média"
# Se a faixa de desconto foi acima ou igual à e não houve atraso na entrega = "Entrega no Prazo com Desconto Acima da Média"
# Se a faixa de desconto foi abaixo da média e houve atraso na entrega = "Atraso na Entrega com Desconto Abaixo da Média"
# Se a faixa de desconto foi abaixo da média e não houve atraso na entrega = "Entrega no Prazo com Desconto Abaixo da Média"



# Criando nova coluna "faixa_desconto" e preenchendo com valores NA
df_eng$faixa_desconto <- NA

# Alimentando nova coluna
df_eng$faixa_desconto <- ifelse(
  df_eng$desconto >= 12, "Desconto Acima da Média", "Desconto Abaixo da Média"
)
df_eng$faixa_desconto <- as.factor(df_eng$faixa_desconto)

# Visualizando
summary(df_eng$faixa_desconto)


# Criando nova coluna "performance_faixa_desconto" e preenchendo com valores NA
df_eng$performance_faixa_desconto <- NA

# Alimentando nova coluna
df_eng <- df_eng %>% 
  mutate(performance_faixa_desconto = case_when(
    faixa_desconto == "Desconto Acima da Média" & entregue_no_prazo == 0 ~ "Atraso na Entrega com Desconto Acima da Media",
    faixa_desconto == "Desconto Abaixo da Média" & entregue_no_prazo == 0 ~ "Atraso na Entrega com Desconto Abaixo da Media",
    faixa_desconto == "Desconto Acima da Média" & entregue_no_prazo == 1 ~ "Entrega no Prazo com Desconto Acima da Media",
    faixa_desconto == "Desconto Abaixo da Média" & entregue_no_prazo == 1 ~ "Entrega no Prazo com Desconto Abaixo da Media",
    TRUE ~ NA
  ))
df_eng$performance_faixa_desconto <- as.factor(df_eng$performance_faixa_desconto)

# Visualizando
summary(df_eng$performance_faixa_desconto)


## Criando um novo DataFrame de Análise para a nova variável performance_modo_envio

# Gerando um dataframe com as análises
df_report3 <- df_eng %>% 
  group_by(performance_faixa_desconto, entregue_no_prazo) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  as.data.frame()
df_report3

# Aplicando Pivot ("girando os dados", transformando linhas em colunas e colunas em linhas)
df_report3 <- df_report3 %>%
  pivot_wider(names_from = entregue_no_prazo, 
              values_from = contagem) %>% 
  as.data.frame()
df_report3

# Renomenado Colunas
names(df_report3) <- c('Status_do_Envio', 'Total_Atraso', 'Total_no_Prazo')
df_report3

# Substituindo NA por zero
df_report3 <- df_report3 %>%
  mutate(Total_Atraso = if_else(is.na(Total_Atraso), 0, Total_Atraso),
         Total_no_Prazo = if_else(is.na(Total_no_Prazo), 0, Total_no_Prazo))

# Concatenando as colunas "Total Atraso" e "Total no Prazo" para criar uma nova coluna "Total" e remove as colunas "Total Atraso" e "Total no Prazo"
df_report3 <- df_report3 %>%
  mutate(Total = Total_Atraso + Total_no_Prazo) %>% 
  select(-Total_Atraso, -Total_no_Prazo) %>% 
  as.data.frame()
df_report3
str(df_report3)

# Gráfico de Barras
ggplot(df_report3, aes(x = Status_do_Envio, y = Total, fill = Status_do_Envio)) +
  geom_col() +  # Usamos geom_col() para valores pré-tabulados
  labs(x = "Status do Envio", y = "Total") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set1")  # Usando uma paleta de cores para valores discretos



## Salvando o dataset
write.csv(df_eng, "dados/df_eng.csv", row.names = FALSE)

