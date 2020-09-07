#### Bibliotecas
install.packages("caret")
install.packages ("readxl")
install.packages ("C50")
install.packages ("e1071")

library(datasets)
library(readxl)       
library(ggplot2)
library(C50)
library(e1071)

##### Pegar os diretórios autais
getwd(); list.files(full.names = TRUE, recursive = TRUE)

##### Estatística Quantitativa
str(iris)


summary(iris)


#box plot por característica
par(mar=c(7,5,1,1))
boxplot(iris,las=2)


#box plot por subconjunto de dados
# criação dos subconjuntos de dados
irisVer <- subset(iris, Species == "versicolor")
irisSet <- subset(iris, Species == "setosa")
irisVir <- subset(iris, Species == "virginica")

# criação do box plot
par(mfrow=c(1,3),mar=c(6,3,2,1))
boxplot(irisVer[,1:4], main="Versicolor",ylim = c(0,8),las=2)
boxplot(irisSet[,1:4], main="Setosa",ylim = c(0,8),las=2)
boxplot(irisVir[,1:4], main="Virginica",ylim = c(0,8),las=2)


# criação do histograma
hist(iris$Petal.Length)

par(mfrow=c(1,3))
hist(irisVer$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))
hist(irisSet$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))
hist(irisVir$Petal.Length,breaks=seq(0,8,l=17),xlim=c(0,8),ylim=c(0,40))


#Análise de correlação
corr <- cor(iris[,1:4])
round(corr,3)

pairs(iris[,1:4],col=iris[,5],oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.85,0.6, as.vector(unique(iris$Species)),fill=c(1,2,3))


##### Estatística Qualitativa

dataset <- read_excel('./Documents/Aulas/futebol.xlsx')
dados =  read.csv('./Documents/Aulas/covid.csv',header = T)


table(dataset$Winning.Team)
table(dataset$Winning.Team, dataset$Competition)

tabela_1 <- table(dataset$Winning.Team, dataset$Competition,dataset$Criterion1.shot)
prop.table(tabela_1)
ftable(round(prop.table(tabela_1), 3))


ggplot(dataset, aes(x = Winning.Team)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# re-order levels
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(dataset, aes(x = reorder_size(Winning.Team))) +
  geom_bar() +
  xlab("Vencedores") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dataset, aes(x = reorder_size(Winning.Team))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Vencedores") +
  scale_y_continuous(labels = scales::percent, name = "Proporção") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(dataset, aes(x = reorder_size(Winning.Team))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Vencedores") +
  scale_y_continuous(labels = scales::percent, name = "Proporção") +
  facet_grid(~ Competition) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(dataset, aes(x = reorder_size(Winning.Team))) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  xlab("Vencedores") +
  scale_y_continuous(labels = scales::percent, name = "Proporção") +
  facet_grid( Criterion1.shot ~ Competition) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
