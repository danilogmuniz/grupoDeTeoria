#==========================================================
#Kokko Cap 6 - Game Theory (Teoria de Jogos)
#==========================================================

#Podemos fazer um plot descrevendo a ESS em funcao de algum
#valor de parametro?
#Sim. A verdadeira questao é: como?

#Pois bem,o primeiro passo eh encapsular o procedimento que
#tinhamos usado originalmente pra achar a ESS em uma funcao!
#De preferencia uma funcao que nos devolva a ESS!!

#----------------------------------------------------------
#essa funcao soh encapsula trechos de codigo que ja usamos
#no outro script ("Kokko Cap 6 - Game Theory.r")

#funcao que calcula o fitness de uma arvore com h1, competindo
#com uma vizinha que tem h2
fit1 = function(h1, h2, alpha=3, Pl=0.25, Ph=1)
{
  delta_h = h1-h2
  f1 = 1-(h1^alpha)
  g1 = Pl+(Ph-Pl)/(1+exp(-5*delta_h))
  return(f1*g1)
  
}
#----------------------------------------------------------
###arvoreESS - funcao que encontra a ESS no modelo de crescimento
###de arvores do capitulo 6 do livro da Kokko.

#os argumentos da funcao sao:
#alpha, Pl e Ph sao os parametros do modelo
#res eh a "resolucao" dos calculos, a ESS eh encontrada
#com precisao de 1/res
#plot eh um argumento logico perguntando se eh pra plotar um
#grafico
arvoreESS = function(alpha, Pl, Ph, res=1001, plot=TRUE)
{
  
  #valores de x individuo 2
  xvals = seq(from = 0, to=1, length.out=res)
  
  #valores y, vamos descobrir qual o melhor y contra
  #valor de x
  yvals = xvals
  
  #matriz de fitness de todo mundo contra todo mundo
  #(passando os argumentos alpha, Pl e Ph para a funcao fit1)
  fitmatrix = outer(xvals, yvals, fit1, alpha=alpha, Pl=Pl, Ph=Ph)

  #descobrindo a posicao do y que da o maior fitness contra cada x
  which_opt = apply(fitmatrix, 2, which.max)
  
  #recuperando o valor de y usando indexacao
  besty = yvals[which_opt]
  which_ess = which(besty==xvals)[1]
  #esse indexado [1] eh porque teve uns casos de dois valores muito proximos empatarem
  ess_value = besty[which_ess]
  
  #par(bty="o", las=1)
  if (plot)
  {
    plot(besty~xvals, ylim=c(0,1), type = "l", xaxs="i", yaxs="i",
         xlab = "Height of plant A", ylab="Height of plant B") 
    points(xvals~besty, type = "l", lty=2)    
  }
  invisible(list(ess=ess_value, dat=cbind(h2 = xvals, besth1=besty)))
}
#----------------------------------------------------------
#agora podemos testar nossa funcao

#e assim conseguimos reproduzir as figuras 6.5 A e B
arvoreESS(alpha=2, Pl=0.2, Ph=0.8)
arvoreESS(alpha=2, Pl=0.8, Ph=1)

#ok, legal. E um plot legal de altura em funcao de alpha?

alphas = seq(from = 1, to=10, length.out = 100) #vetor de valores de alpha
ess_vector = rep(NA, length(alphas)) #vetor pra guardar a ESS em cada situacao

#percorrendo os alphas e guardando a ESS em cada caso
for(i in 1:length(alphas))
{
  arvore = arvoreESS(alpha=alphas[i], Pl=0.25, Ph=1.0, plot=FALSE)
  ess_vector[i] = arvore$ess
  
}
plot(ess_vector~alphas, ylab="evolutionary stable tree height")

#e a principio, podemos fazer o mesmo pro Pl
Pls = seq(from=0.1, to=0.9, length.out = 100)
ess_pl = rep(NA, length(Pls))

for(i in 1:length(Pls))
{
  arvore = arvoreESS(alpha=3, Pl=Pls[i], Ph=1.0, plot=FALSE)
  ess_pl[i] = arvore$ess
  
}
plot(ess_pl~Pls, ylab="evolutionary stable tree height")

#----------------------------------------------------------
#tentando chegar num grafico de previsoes que seja mais
#facil de entender do que olhar pras equacoes

(Plvals = seq(0.2, 0.8, length.out=4))
(alphavals = seq(from=1, to=5, length.out=21))

#criando o data.frame de dados que vamos usar pra plotar depois
dat = expand.grid(Pl=Plvals, alpha = alphavals)
dat$ESS = NA

for(i in 1:nrow(dat))
{
  arvore = arvoreESS(alpha=dat$alpha[i], 
                     Pl=dat$Pl[i], Ph=1.0, plot=FALSE)
  dat$ESS[i] = arvore$ess
  
}
head(dat)

#truque sujo pra facilitar o plot
mdat = unstack(dat, ESS~Pl)
head(mdat)

#hora do plot
cores = c('#d7191c','#fdae61','#abdda4','#2b83ba') #hex power!

par(las=1, bty="l")
matplot(y=mdat, x=alphavals, type = "l", col=cores, pch=19, 
        lty=1, lwd=2, ylab="Plant height", xlab="alpha (decreasing stem cost)")
legend("bottomright", col=cores, lty=1, lwd=2,bty="n",
       legend = Plvals, title = "Pl value\n(decreasing shading cost)")

#OK, apesar de termos nos perdido absurdamente na interpretacao
#da ESS em funcao das derivadas das funcoes. Olhando para o 
#grafico e pensando biologicamente, o modelo faz sentido:
#quanto maior o alpha, ou seja, quanto mais barato eh ter
#um tronco alto, mais altas sao as plantas.
#Alem disso, quanto maior o Pl, ou seja, quanto menos custoso
#eh ficar na sombra, mais baixinhas sao as plantas.

#A forma como o modelo eh pensado, com o custo diminuindo conforme
#os parametros aumentam, deixa a coisa bem confusa de entender.
#Mas no final das contas, faz sentido.


#==========================================================

