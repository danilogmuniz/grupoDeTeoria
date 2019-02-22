#====================================================================
#Kokko cap 4 - otimizacao
##########################

#Primeiro modelinho, em que o fitness eh igual ao beneficio do ornamento
#do macho menos o custo

#Vamos tentar reproduzir os graficos do livro

#criando uma sequencia de valores de tamanho de ornamento
#(nao podemos usar T como nome de variavel, entao usasei Z)
Z = seq(from=0, to=1, length.out = 1001)#valores de ornamento
b = Z #valores do beneficio

alphaL = 2
alphaH = 1

#calculando os custos
cL = alphaL*(Z^2)
cH = alphaH*(Z^2)

#figura 4.2
par(las=1, bty="l")
cols = c("dodgerblue", "firebrick", "darkgreen")
matplot(x=Z, y=cbind(b, cL, cH), type="l", lty=1, col=cols,lwd=2,
        xlab = "Male trait, T", ylab = "Benefits and costs")

#agora, calculando os valores de fitness
wL = b-cL; wH = b-cH
positions = which(Z<=1)
matplot(x=Z[positions], y=cbind(wL[positions], wH[positions]), 
        type="l", lty=1, col=cols[1:2], lwd=4,
        xlab = "Male trait, T", ylab = "Net benefit", ylim=c(0,0.3))

#procurando os valores otimos
Z[wL == max(wL)]
Z[wH == max(wH)]
#note que os valores batem com o calculado analiticamente

#podemos adicionar isso no figura
abline(v = Z[wL == max(wL)], col=cols[1], lty=2)
abline(v = Z[wH == max(wH)], col=cols[2], lty=2)
#====================================================================
#Segundo modelinho, mais "life history based"
##############################################

#o código que eu fiz funciona um pouco diferente do da Kokko
#(nao eh uma traducao direta), mas o resultado final eh parecido
#A funcao maledisplay() recebe um vetor de valores de R e calcula
#o fitness de individuos com aquele R para varios valores de a
#(investimento no trait sexual) entre zero e um. O numero de valores
#de a "testados" eh determinado pelo argumento res (de resolucao).
#A funcao retorna uma matriz com os valores de apticao para todas
#as combinacoes de valores de R e valores de a testadas, alem de
#um data.frame resumindo a estrategia otima para cada valor de R.


#Antes da funcao principal, temos essas duas funcoes envolvidas
#no calculo da probabilidade de sobrevivencia ate a proxima
#estacao reprodutiva.

invlogit = function(x){1/(1+exp(-x))} #inverse logit function
surv = function(x){invlogit(10*(x-0.5))} #survival function
curve(surv(x=x), from=0, to=1)#ok!!

#agora sim a funcao maledisplay()
maledisplay = function(R = c(0.1, 0.2, 0.5), res=1001)
{
  #valores de estrategia de alocacao alpha que serao
  #varridos pelo codigo
  avalues = seq(from=0, to=1, length.out = res) 
  
  #matriz de valores de fitness
  #cada linha eh uma estrategia de alocacao (valor de alpha)
  #e cada coluna eh um valor de R (recurso disponivel)
  fitness = matrix(NA, nrow=res, ncol=length(R))
  
  #matriz de valores otimos de a para cada R, armazena
  #tambem o valor do ormanento: Z, a probabilidade de sobrevivencia,
  #o numero de estacoes que bicho sobrevive: seasons 
  #e a aptidao final/total: W
  optvals = matrix(NA, ncol=5, nrow=length(R))
  colnames(optvals) = c("a","Z","survival","seasons","W")
  
  #cada rodada do for eh um valor de R, onde a gente calcula
  #o fitness para todos os alphavalues dado aquele valor de R
  for(i in 1:length(R))
  {
    #valores de ornamento para cada alphavalue dado o R atual (posicao i)
    zvalues = avalues*R[i] 
    
    #valores de condicao (ou investimento em sobrevivencia)
    cvalues = R[i]-zvalues 
    
    #probabilidade de sobrevivencia
    survival = surv(cvalues) 
    
    #numero de estacoes vividas para cada alphavalue
    seasons = survival/(1-survival) 
    
    #valores totais de aptidao
    W = zvalues + zvalues*seasons #fitness total
    
    #armazena os valores de fitness 
    fitness[,i] = W 
    
    #o ultimo passo eh descobrir o valor otimo de alpha para cada R
    #aqui eu ja aproveitei pra guardar tambem o fitness maximo,
    #sobrevivencia e valor de ornamento t quando o alpha esta no otimo
    which.opt = which.max(W) #posicao de fitness maximo (no vetor)
    
    optvals[i,] = c(avalues[which.opt], zvalues[which.opt],
                    survival[which.opt],
                    seasons[which.opt], W[which.opt])
    
  }
  
  #retorna tudo
  return(list(a=avalues, w=fitness, R = R, optvals=as.data.frame(optvals)))
}

rvals = c(0.2, 0.4, 0.6, 0.8)
teste = maledisplay(R = rvals)

#curvas de aptidao em funcao do investimento reprodutivo (a) para machos
#com diferentes valores de R (a Kokko nao mostra isso)
par(las=1, bty="l", oma=c(0,0,0,0), cex=1)
matplot(x=teste$a, y=teste$w, type="l", lwd=3, lty=1, col=rainbow(length(rvals)),
        xlab = "Mating effort (a)", ylab = "Fitness", ylim=c(0,1))

legend(y = 1.2, x=0.5, xjust=0.5, lty=1, lwd=3, legend = rvals, title = "R value", 
       col=rainbow(length(rvals)),bty="n",horiz = TRUE, xpd=TRUE)

#agora vamos montar os graficos da figura 4.9 da Kokko
#e um plot bonus
par(mfrow=c(2,2), las=1, bty="l")

dados = maledisplay(R = seq(from=0.1, to=1.0, by=0.01), res=1001)

plot(x=dados$R, y=dados$optvals$Z, type="l", lwd=2,
     xlab="Male condition R", ylab = "Male ornament")

plot(x=dados$R, y=dados$optvals$survival, type="l", lwd=2,
     xlab="Male condition R", ylab = "Male survival probability")

plot(x=dados$R, y=dados$optvals$seasons, type="l", lwd=2,
     xlab="Male condition R", ylab = "Number of (additional) seasons")
plot(x=dados$R, y=dados$optvals$W, type="l", lwd=2,
     xlab="Male condition R", ylab = "Fitness")

#====================================================================
