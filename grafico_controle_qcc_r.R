#Chama o pacote qcc usado para controle estatistico de processos
library(qcc)

###################
# Fabrica de an?is de pist?o pra motor de carros em processo de forjamento.
# Fase 1: 25 amostras com 5 unidades cada sao coletadas para medir o di?metro interno dos an?is 
# Fase 2: 15 amostras com 5 unidades cada sao coletadas para medir o di?metro interno dos an?is 
# O conjunto de dados tem 3 variaveis diameter (continua), sample(Nominal ou discreta), e trial (binaria)

####Usando a base de dados pistonrings disponivel no pacote qcc
library(qcc)
library(tidyverse)
data("pistonrings")

#Ajustando a tabela para construcao do grafico XR
diametro <- qcc.groups(pistonrings$diameter, pistonrings$sample)

#Gerando gr?fico com dados da Fase1 com 25 primeiras e acrescentando dados da Fase 2 com 15 amostras 
grafico_1 <- qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,])

####### Extras para quem tiver interesse no assunto

#Medindo a capacidade do processo
capacidade = qcc(diametro[1:25,], type="xbar", nsigmas=3, plot=FALSE)
process.capability(capacidade, spec.limits=c(73.95,74.05))


#Medindo a curva 
beta <- oc.curves.xbar(qcc(diametro, type="xbar", nsigmas=3, plot=FALSE))


#Separando os 2 graficos
par(mfrow=c(1,1))
plot(grafico_1, restore =F)
plot(grafico_1, chart.all =F)


#Retirando algumas amostras do conjunto
out <- c(9, 10, 30, 35, 45, 64, 65, 74, 75, 85, 99, 100)
diametro2 <- qcc.groups(pistonrings$diameter[-out], sample[-out])
qcc(diametro2[1:25,], type="xbar")

#Colocando regras de Western Eletric
grafico_western <- qcc(diametro2[1:25,], type = "xbar", rules = 1:4 )


#Colocando o numero de sigmas pra 2
grafico_sigma <- qcc(diametro2[1:25,], type = "xbar", newdata = diametro2[26:40,], nsigmas = 2)

#Aumentando o intervalo de confianca para 99%
grafico_confianca <- qcc(diametro[1:25,], type = "xbar", newdata = diametro[26:40,], confidence.level = 0.99)

# Colocando um aviso quando ultrapassar 2 desvios padrao
q <- qcc(diametro2[1:25,], type="xbar", newdata=diametro2[26:40,], plot=FALSE)
(warn.limits <- limits.xbar(q$center, q$std.dev, q$sizes, 2))
plot(q, restore.par = FALSE)
abline(h = warn.limits, lty = 3, col = "chocolate")

detach(pistonrings)


#Chama o pacote qcc usado para controle estatistico de processos
library(qcc)

#Fabrica de caixa longa vida de 125 ml para Suco de laranja
#S?o formadas em uma m?quina que gira o papelao e insere um monte de camadas dentre elas uma de metal
#Fazemos inspecao pra observar se ao preenchida poderia vazar na costura lateral ou na junta inferior.
#Se isso ocorrer, uma caixa ? considerada n?o conforme. 
#Coletou se 30 amostras de 50 cxs cada a cada meia hora e durante tr?s turnos operando continuamente
#Na amostra 15, um novo lote de papel?o foi colocado em produ??o.
#Na amostra 23  um operador inexperiente foi temporariamente convocado pra operar a m?quina. 
#Depois de coletar as 30, foi feito um ajuste na m?quina e fizemos mais 24 amostras do processo.
#Variaveis Sample (Sequencia), D (qtd defeitos), sample_size (tamanho da amostra), trial (TrueFalse)


#Quantidade de pecas defeituosas

##  Grafico por atributos
data(orangejuice)
attach(orangejuice)
?orangejuice

boxplot(D ~ trial)

#Constroi o gr?fico P onde Trial = TRUE ou seja, as primeiras 30 amostras
qcc(D[trial], sizes=size[trial], type="p")


####### Extras para quem tiver interesse no assunto

# Remover os dois pontos fora do controle 15- Lote de papelao 23- Operador Inexperiente
#qcc(variavel estudada, tamanho da amostra, tipo de grafico, se tem dado novo ou nao, regras )
novo_dataset <- setdiff(which(trial), c(15,23))
q1 <- qcc(D[novo_dataset], sizes=size[novo_dataset], type="p")


qcc(D[novo_dataset], sizes=size[novo_dataset], type="p", newdata=D[!trial], newsizes=size[!trial]) 



# Opcao 2 para Remover os dois pontos fora do controle 15- Lote de papelao 23- Operador Inexperiente
q1 <-qcc(D[trial], size = size[trial], type ="c", newdata=D[!trial], newsizes=size[!trial])
q2 <-qcc(D[c(1:30)[-c(15,23)]], size = size[c(1:30)[-c(15,23)]], type ="c", newdata=D[!trial], newsizes=size[!trial])

par(mfrow=c(2,1))
plot(q1, restore =F)
plot(q2)

detach(orangejuice)

#Agregando dataset da amostra 2 
data(orangejuice2)
attach(orangejuice2)
names(D) <- sample
qcc(D[trial], sizes=size[trial], type="p")
q2 <- qcc(D[trial], sizes=size[trial], type="p", newdata=D[!trial], newsizes=size[!trial])
detach(orangejuice2)

# put on the same graph the two orange juice samples
oldpar <- par(no.readonly = TRUE)
par(mfrow=c(1,2), mar=c(5,5,3,0))
plot(q1, title="First samples", ylim=c(0,0.5), add.stats=FALSE, restore.par=FALSE)
par("mar"=c(5,0,3,3), yaxt="n")
plot(q2, title="Second samples", add.stats=FALSE, ylim=c(0,0.5))
par(oldpar)

detach(orangejuice)
detach(orangejuice2)


#Grafico C por numero de defeitos numa peca

#Fabrica de placas de circuito impresso 
#Coletamos 26 amostras sucessivas de 100 placas de circuito impresso e medimos o n?mero de n?o conformidades em cada placa
#As amostras 6 e 20 est?o fora dos limites de controle
#A 6 foi examinada por um novo inspetor e ele n?o reconheceu v?rios tipos NC que poderiam estar presentes.
#A 20 foi resultado de um problema de controle de temperatura na m?quina de solda por onda, que foi posteriormente reparada. 
#As ?ltimas 20 amostras s?o coletadas em unidades de inspe??o (cada uma formada por 100 placas). 


data(circuit)
attach(circuit)
?circuit
qcc(x[trial], sizes=size[trial], type="c")

# remove out-of-control points (see help(circuit) for the reasons)
inc <- setdiff(which(trial), c(6,20))

qcc(x[inc], sizes=size[inc], type="c", labels=inc)

qcc(x[inc], sizes=size[inc], type="c", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
qcc(x[inc], sizes=size[inc], type="u", labels=inc, 
    newdata=x[!trial], newsizes=size[!trial], newlabels=which(!trial))
detach(circuit)

data(pcmanufact)
attach(pcmanufact)
qcc(x, sizes=size, type="u")
detach(pcmanufact)
















data(dyedcloth)
attach(dyedcloth)
qcc(x, sizes=size, type="u")
# standardized control chart
q <- qcc(x, sizes=size, type="u", plot=FALSE)
z <- (q$statistics - q$center)/sqrt(q$center/q$size)
plot(z,  type="o", ylim=range(z,3,-3), pch=16)
abline(h=0, lty=2)
abline(h=c(-3,3), lty=2)
detach(dyedcloth)


##  Continuous one-at-time data 
##

# viscosity data (Montgomery, pag. 242)
x <- c(33.75, 33.05, 34, 33.81, 33.46, 34.02, 33.68, 33.27, 33.49, 33.20,
       33.62, 33.00, 33.54, 33.12, 33.84)
g<- qcc(x, type="xbar.one")
c<-qcc(x, type="xbar.one", std.dev = "SD")

plot(c)

#Pontos aleat?rios com m?dia em 10 e seguindo a distribui??o normal
parafuso_a <- rep(10, 50) + rnorm(50, mean=0, sd=0.5)
grafico_1  <- qcc(parafuso_a, 
                  type="xbar.one", 
                  center=10, 
                  add.stats=FALSE,
                  title="Lote 1",
                  xlab="Parafusos produzidos")

# Primeiros 90 pontos aleat?rios com m?dia em 10 e seguindo a distribui??o normal
# Ultimos   10 pontos aleat?rios com m?dia em 11 e seguindo a distribui??o normal
parafuso_b <- c(rep(10, 40), rep(11, 10)) + rnorm(40, mean=0, sd=0.5)
grafico_2  <- qcc(parafuso_b, 
                  type="xbar.one", 
                  center=10, 
                  add.stats=T,
                  title="Lote 2",
                  xlab="Parafusos produzidos")

#Agora usando conjunto de valida??o e testes
parafuso_c <- rep(10, 50) + rnorm(50, mean=0, sd=0.5)
grafico_3  <- qcc(parafuso_c, 
                  newdata=rep(9, 20) + rnorm(20, mean=0, sd=0.2),
                  type="xbar.one", 
                  center=10, 
                  add.stats=FALSE, 
                  title="Lote 2", 
                  xlab="Parafusos produzidos")

