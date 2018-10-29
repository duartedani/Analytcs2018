### Exercício - Revisão
### Regressão linear - Regressão logística

install.packages("dplyr")
install.packages("ggplot2")
install.packages("texreg")
install.packages("descr")
install.packages("haven")

library(dplyr)
library(ggplot2)
library(texreg)
library(descr)
library(haven)

bd = mtcars
names(bd)
View(bd)

# Parte A - regressão linear

# Monte 3 modelos de regressão. Todos eles devem explicar o consumo do carro (mpg) através de seus atributos.
# No primeiro, utilize o peso do carro (wt) para explicar o consumo. No segundo, use a potência em cavalos (hp)
# Na terceira use as duas variáveis juntas. Apresente uma tabela com os resultados das 3 regressões juntas.

# Utiliza-se regressão linear, pois a variavel mpg não é categorica
reg = lm (mpg ~ wt, data = bd)
summary(reg)

#centralizar na média 
#Foi criado uma nova variável, wtcent
# Pega o valor da variavel que quero centralizar e retira a media
bd$wtcent = bd$wt - mean(bd$wt)
bd$wtcent

# Uma nova regressão com o valor centralizado na média
reg2 = lm (mpg ~ wtcent,data =bd)
summary(reg2)

# Interpretação
# o Consumo esperado de um veiculo de peso médio (3.21) é de 20.09 galões em média
# para um carro com peso médio de (3.21)espera-se um consumo médio de 20,09 galões.

#potência em cavalos (hp) - Primeiro coloco o Y ~ X
bd$hpcent = bd$hp - mean(bd$hp)
bd$hpcent
reg3 = lm (mpg ~ hpcent, data = bd)
summary(reg3)
  
# Interpretação
# O consumo médio esperado de um veiculo cuja a potencia média é 146.68hp é de 20.09 galões em média

#Duas variáveis juntas 
#Formula = y=b0 + b1x1 + b2x2 + n 
reg4= lm (mpg ~ hpcent + wtcent,data = bd)
summary(reg4)
# Interpretação
# O consumo médio esperado de um veiculo cuja a potencia média é 146.68hp e o peso médio médio (3,21) é de 20.09 galões em média
# Para o aumento de 1 unidade (1000 libras) no peso(wt) espera-se uma diminuição média do consumo de 3,87 milhas/galão (mpg)
# Para o aumento de 1 unidade (hp) de potencia espera-se uma diminuição média de 0.03 milhas /galão (mpg)

#tabela com os resultados das 3 regressões juntas.
screenreg(list(reg2,reg3,reg4))
