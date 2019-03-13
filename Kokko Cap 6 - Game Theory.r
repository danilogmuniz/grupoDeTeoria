#==========================================================
#Kokko Cap 6 - Game Theory (Teoria de Jogos)
#==========================================================

#Modelinho de crescimento de arvores

#Curvas de f (proporcao de tecido fotossintetizante) em funcao
#de h (altura da planta)

#a funcao eh f(h) = 1-h^alpha
#valores de alpha: 2, 3, 10

fh = function(h, alpha)
{
  1-(h^alpha)
}

#desenhando as curvas de f em funcao de h
par(las=1, bty="l")
curve(fh(h=x, alpha=10), from=0, to=1, col="black", lwd=2,
      xlab = "Plant height, h", ylab = "Proportion of leaf tissue, f(h)")
curve(fh(h=x, alpha=3), col = "dodgerblue", lwd=2,add=TRUE)
curve(fh(h=x, alpha=2), col = "firebrick", lwd=2, add=TRUE)


#curva de eficiencia de fotossintese em funcao do delta_h

#funcao g(delta_h)
gdh = function(delta_h, Pl, Ph)
{
  Pl+(Ph-Pl)/(1+exp(-5*delta_h))
}


curve(gdh(delta_h=x, Pl=0.8, Ph =1), from=-1, to=1, lwd=2,
      ylim=c(0,1),
      xlab="Difference in height", ylab = "Photosynthesis/leaf")
curve(gdh(delta_h = x, Pl=0.2, Ph=0.8), add=TRUE, lwd=2, col="dodgerblue")


#----------------------------------------------------------
#tentando montar essa matriz de payoffs

#Vamos montar uma funcao que calcula o fitness de uma planta
#com h1 quando esta competindo com uma planta com h2. 
#Para facilitar na hora de calcular a matriz de payoffs, 
#o ideal eh que a funcao seja vetorizada, ou seja, 
#consiga se virar mesmo se h1 e h2 forem vetores

fit1 = function(h1, h2, alpha=3, Pl=0.25, Ph=1)
{
  delta_h = h1-h2
  f1 = 1-(h1^alpha)
  g1 = Pl+(Ph-Pl)/(1+exp(-5*delta_h))
  return(f1*g1)
  
}

#testando
fit1(h1=c(0, 1/3, 2/3, 1), h2=0) #it funfs!

#Com isso podemos montar a matriz usando a incrivel funcao outer()

hvals = c(A=0, B=1/3, C=2/3, D = 1)
payoffs = outer(hvals, hvals, fit1)
payoffs #tcharans!!

#Nessa matriz, cada celula representa o fitness da estrategia
#da linha quando confrontada com a estrategia da coluna.
#Com isso, soh precisamos de um valor por celula

#Em busca da ESS

#D eh invadido por todo mundo

#A invade B?
#(A aptidao de um mutante A versus um vizinho B
#eh maior que a aptidao de um B versus outro B?)
payoffs["A", "B"] > payoffs["B", "B"]
#Nao

#B invade A?
payoffs["B", "A"] > payoffs["A", "A"]
#Invade, logo A nao pode ser ESS

#B invade C?
payoffs["B", "C"] > payoffs["C", "C"]
#Nao

#C invade B?
payoffs["C", "B"] > payoffs["B", "B"]
#Mas C invade B. Logo B nao pode ser ESS.

#Se A tambem nao invadir C, mas C invadir A,
#ai C eh a ESS
payoffs["A", "C"] > payoffs["C", "C"] #A nao invade C
payoffs["C", "A"] > payoffs["A", "A"] #C invade A

#Logo C eh a ESS. Ele invade todo mundo, mas ninguem
#invade ele.

#----------------------------------------------------------
#Bem, mas nessa historia nao faz realmente sentido pensar
#em estrategias discretas. A coisa eh continua. Vamos entao
#tentar montar aquele grafico bonitinho da figura 6.4b
#(e achar a ESS!!)

#(Na verdade, vamos testar mais de mil estrategias discretas
#e considerar que isso eh uma aproximacao boa do mundo continuo)

#numero de estragerias que vamos testar
N = 1001 

#valores de x que vao pro eixo x (estrategia residente)
xvals = seq(from = 0, to=1, length.out=N)

#valores y, vamos descobrir qual o melhor y contra
#valor de x
yvals = xvals
besty = rep(NA, length(xvals))

for(i in 1:length(xvals))
{
  #calcula o fitness de todas as N estrategias
  #contra a estrategia focal i
  fitvector = fit1(h1=yvals, h2=xvals[i])
  
  #descobre a posicao do valor otimo
  which_opt = which.max(fitvector)
  
  #guarda esse valor no vetor besty
  besty[i] = yvals[which_opt]
}

#e agora eh hora de por no grafico
par(bty="o", las=1)
plot(besty~xvals, ylim=c(0,1), type = "l", xaxs="i", yaxs="i") 
points(xvals~besty, type = "l", lty=2)    

#nesse caso, o jeito de achar a ESS eh o valor em que x e y tem a
#mesma aptidao
ess = xvals[which(besty == xvals)]
points(x=ess, y=ess, col="red", cex=3)


#----------------------------------------------------------
#######
#BONUS.
#######

#Vamos tentar fazer a mesma brincadeira sem usar o 
#for (soh na base do outer e do apply)

N = 1001 #numero de estragerias que vamos testar
#valores de x que vao pro eixo x (estrategia residente)
xvals = seq(from = 0, to=1, length.out=N)

#valores y, vamos descobrir qual o melhor y contra
#valor de x
yvals = xvals

#matriz de aptidao de cada valor de y contra cada valor de x
fitmatrix = outer(xvals, yvals, fit1)

#descobrindo a posicao do y que da o maior fitness contra cada x
which_opt = apply(fitmatrix, 2, which.max)

#recuperando o valor de y usando indexacao
besty = yvals[which_opt]

par(bty="o", las=1)
plot(besty~xvals, ylim=c(0,1), type = "l", xaxs="i", yaxs="i") 
points(xvals~besty, type = "l", lty=2)    

ess = xvals[which(besty == xvals)]
points(x=ess, y=ess, col="red", cex=3)
#e temos o mesmo grafico!

#==========================================================