# probC.R - é um programa para análise de probabilidade condicional
# Copyright (c) 2021 Henrique Regis Costa <henrique260293@gmail.com>
# Copyright (c) 2021 Thaís Regis Costa <thasregiscosta@gmail.com>
# Este arquivo é distribuido sob a licença GPLv3.

#carga da base de dados e atribuição da mesma para a variável 't'(table) (formato CSV UTF-8)
t <- read.csv("C:/Users/Henrique/Desktop/probabilidade/problema/data_set_problema.csv", header=TRUE)

#exibe no console o conteúdo da variável 't'
t

#atribui à 'a' a tabela de probabilidades oriunda da tabela de contagem (contingência) contida na variável 't'
a <- prop.table(t)
#exibe no console o conteúdo da variável 'a'
a

#tranforma 'a' em uma matriz e atribui à variável 'b'
b <- as.matrix(a)
#exibe no console o conteúdo da variável 'b'
b

#gera tabela 'mDist' que é b com somas marginais
mDist <- addmargins(b)

#armazena em 'd' a tabela de probabilidades condicionais obtida a partir da tabela de probabilidades contida na variável 'a'
# margin => soma marginal | margin = 1 condição na "dimensão 1" => X | margin = 2 condição na "dimensão 2" => Y
d <- prop.table(b, margin=2)
#exibe no console o conteúdo da variável 'd'
d

#atribui à c (colum) o número de colunas da matriz d, l(line) o número de linhas da matriz d
c <- as.integer(ncol(d))
l <- as.integer(nrow(d))
#exibe no console os conteúdos das variáveis c e l
c
l

#iterações para geração de E e VAR

#contadores iniciados com valor 1 para realizar iterações até um n-ésimo valor desejado
i <- 1
j <- 1
k <- 1
m <- 1

#vetores de tamanho "c - número de colunas" para acumular resultados
e <- 1:c
e2 <- 1:c
var <- 1:c

#Iteração para cálculo de E e Var

#enquanto o contador k estiver entre 1 e c - número de colunas executa as ações que estão entre as chaves do laço externo
for(k in 1:c){
  
  #atribuição de valor 0 na posição k-ésima do vetor 'e' (objetivo de evitar "resíduos")
  e[k] = 0
  
  #laço da 'expectation' aplica a fórmula para cada linha e acumula um novo valor ao k-ésimo 'e' a cada iteração
  for(i in 1:l){
    e[k] = e[k] + ((i) * d[i, k])
  }
  
  #atribuição de valor 0 na posição k-ésima do vetor 'e2' (objetivo de evitar "resíduos")
  e2[k] = 0
  
  #laço do 'e^2' aplica a fórmula para cada linha e acumula um novo valor ao k-ésimo 'e2' a cada iteração
  for(j in 1:l){
    e2[k] = e2[k] + ((j)^2 * d[j, k])
  }
}

#a partir de 'e' e 'e2', calcula Var da posição m = 1 até c - número de colunas e acumula no vetor var da posição m =1 até c 
for(m in 1:c){
  var[m] = 0
  var[m] = e2[m] - e[m]^2
}

#exibe no console resultados de expectation e var
e
var

#Iteração para criação dos gráficos das distribuições marginais
n <- 1
o <- 1
yMg <- 0
xMg <- 0
x <- 1:c
y <- 1:l

#laço que cria vetor P(X)
for(n in 1:c){
  xMg[n] = 0
  xMg[n] = mDist[l+1,n]
}

#laço que cria vetor P(Y)
for(o in 1:l){
  yMg[o] = 0
  yMg[o] = mDist[o,c+1]
}

#Criação de tabela com valores de x e seus respectivos P(x)
xplot <- as.table(setNames(xMg,x))
#Exibe tabela xplot
xplot
#Criação de tabela com valores de y e seus respectivos P(y)
yplot <- as.table(setNames(yMg,y))
#Exibe tabela yplot
yplot

#exibe tabela mDist (probabilidade e somas marginais)
mDist 
#plota gráficos das somas marginais
barplot(xplot, ylab="Probabilidade", xlab="valores de X", col="darkblue")
barplot(yplot, ylab="Probabilidade", xlab="valores de y", col="red")

#plota gráfico da distribuição conjunta
barplot(d, beside=TRUE, col=c(1:l))




