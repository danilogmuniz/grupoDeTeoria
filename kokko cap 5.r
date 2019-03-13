#====================================================================
#Kokko capitulo 5 - dynamic optimization
########################################

#Eu tentei traduzir o codigo da Kokko, mas acho que os truques de
#vetorizacao sao diferentes o suficiente entre R e Matlab pra tornar
#a traducao dificil pra quem soh sabe R. Entao eu acabei escrevendo um codigo
#que nao tem nada a ver com o da Kokko, mas que chega no mesmo resultado final.

#A ideia principal do codigo eh que temos dois FORs, um de tempo, e um
#de estados nutricionais. A cada tempo, e a cada estado, a gente calcula
#a melhor coisa a fazer naquela situacao, ignorando o passado.
#Ai, o algoritmo dah um passo para tras no tempo, e recalcula a melhor
#acao para cada estagio nutricional no tempo anterior. E segue assim ateh
#o inicio dos tempos.

#Como o procedimento todo nao eh nada simples, resolvi fazer do jeito
#menos vetorizado possivel, na base do for, if e else mesmo.
#O pseudocodigo mais detalhado ficou:

###inicio do pseudocodigo
#
# Calcula os valores de mortalidade para cada estagio
# Cria a matriz rule que armazena as decisoes para cada estado a cada tempo
# Cria a matriz reward, que armazena o ganho esperado da cada decisao otima
# Cria o vetor gain 
# vetor gain recebe valores de 1 a maxc
# (nesse momento, o vetor gain representa o ganho de chegar ateh a noite no
#  no estado nutricional i)
#
# FOR - Para cada passo de tempo j (partindo do ultimo para o primeiro)
# {
#     FOR - Para cada estado nutricional i (do primeiro para o ultimo)
#     {
#         Calcular o ganho se forrageia e se descansa (usando o vetor gain)
#         (o que ja envolve varios ifs e elses)
#         
#         IF - Se o ganho de forrageio for maior ou igual ao de descanso
#         {
#             guarda a decisao de forragear na matriz rule
#             guarda o ganho por fazer isso na matriz reward
#         }
#            
#         ELSE (Se o ganho de forrageio for menor que o de descanso)
#         {
#             guarda a decisao de descansar na matriz rule
#             guarda o ganho por fazer isso na matriz reward
#         }
#
#    } ##fecha o segundo FOR
#
#    vetor gain recebe a coluna j da matriz reward
#
# } ##fecha o primeiro FOR
#retorna a matriz rule
###fim o pseudocodigo

#Um truque importante eh ir atualizando o vetor gain conforme vamos voltando
#no tempo. A cada rodada, o vetor gain representa o ganho medio esperado caso
#o bicho vah para o estado nutricional i no proximo passo de tempo

#Outra coisa importante/curiosa, eh que eu acabei ignorando totalmente
#os mortos na historia. Quem morre nao pode fazer nada (por definicao),
#e depois que o bicho morre, ele nao ganha nada. Entao, simplesmente
#ignorar os mortos facilitou tudo.

#--------------------------------------------------------------------
#Agora sim, o codigo que roda essa maluquice toda

forageRule = function(dmin = 0,  #mortalidade minima (estado 1)
                      dmax=0.01, #mostalidade maxima (estado maxc)
                      C = 0.4, #probabilidade de cair de estado se descansar
                      f = 0.8, #probabilidade de subir de estado se forragear
                      maxt=5,  #numero de intervalos de tempo
                      maxc=6)  #numero de condicoes nutricionais
{

  #criando os valores de mortalidade para cada estagio
  #esses valores zero no inicio ja vao garantir que o morto fique morto
  d = c(0, seq(from=dmin, to=dmax, length.out = maxc))
  
  #criando o vetor de ganhos por estar no estado nutricional i
  #no proximo passo de tempo (ele vai ser atualizado a cada passo de tempo)
  gain = (1:maxc) 
  
  #matriz de ganhos se fizer a escolha otima em cada posicao
  reward = matrix(0, nrow=maxc, ncol=maxt, byrow=FALSE)
  
  #matriz que armazena as decisoes
  rule = matrix(NA, nrow=maxc, ncol=maxt)
  
  for(j in maxt:1)#percorrendo o tempo do futuro para o passado
  {
    for(i in 1:maxc) #percorrendo os estados nutricionais
    {
      ####calculando os ganhos se forrageia ou descansa
      if(i == maxc)#estamos na condicao maxima
      {
        forageGain = (1-d[i])*gain[i]#se nao morrer, tah no lucro
        restGain = (1-C)*gain[i] + C*gain[i-1] #mas ainda pode cair de condicao
      }
      else if (i == 1)#pior condicao possivel
      {
        forageGain = (1-d[i])*f*gain[i+1]+(1-d[i])*(1-f)*gain[i]
        restGain = (1-C)*gain[i] #se cair de condicao, morre
      }
      else #todas as outras
      {
        forageGain = (1-d[i])*f*gain[i+1]+(1-d[i])*(1-f)*gain[i]
        restGain = (1-C)*gain[i] + C*gain[i-1]
      }
      ####fim dos calculos
      
      ####colocando a conclusao na matriz
      if(forageGain >= restGain)
      {
        rule[i, j] = 1 #armazena a opcao otima
        reward[i, j] = forageGain #guarda o ganho na matriz de ganhos
      }
      else #ou seja restGain > forageGain
      {
        rule[i, j] = 0
        reward[i,j] = restGain
      }
      
    }#fecha o for(i in 1:maxc)
    
    gain = reward[,j]#atualiza o vetor de ganhos para ser usano no proximo tempo
    
  }#fecha o for(j in maxt:1)

 return(rule) 
}

m = forageRule()
m
#it funfs!!!

#vamos tentar plotar essa coisa agora

#--------------------------------------------------------------------

#primeiro passo: funcaozinha marota pra fazer o plot
plotRule = function(x, #matriz rule
                    col = c("white","darkgrey"), #cores de preenchimento
                    lcol = "black", #cor das gridlines
                    xlab="Time", #rotulo do eixo x
                    ylab="Condition", #rotulo do eixo y
                    ...) #outros parametros passados pra funcao image
{

  maxt = ncol(x)
  maxc = nrow(x)
  if(all(x == 1)) #se for tudo 1
    image(t(x), col=col[2], yaxt="n", xaxt="n", 
          xlab = xlab, ylab=ylab, ...)
  else if(all(x == 0)) #se for tudo zero
    image(t(x), col=col[1], yaxt="n", xaxt="n", 
          xlab = xlab, ylab=ylab, ...)
  else #se for uma matriz comportada com zeros e uns
    image(t(x), col=col, yaxt="n", xaxt="n", 
          xlab = xlab, ylab=ylab, ...)
  
  #arrumando a perfumaria do plot
  axis(side=1, at=(0:(maxt-1))/(maxt-1), labels=1:maxt)
  axis(side=2, at=(0:(maxc-1))/(maxc-1), labels=1:maxc)
  grid(nx=maxt, ny=maxc, col=lcol)
  box()
}

#E finalmente, o plot!
par(las=1)
plotRule(m)

#agora vamos tentar reproduzir a figura 5.6

ma = forageRule(dmin = 0, dmax=0, C = 0.4, f = 0.8)
mb = forageRule(dmin = 0, dmax=0.01, C = 0.4, f = 0.8)
mc = forageRule(dmin = 0.01, dmax=0.01, C = 0.4, f = 0.8)

png("kokko 5.6.png", width = 12, height=20, units="cm", res=600)

par(mfrow=c(3,1), las=1,
    cex.lab=2, cex.axis=1.5, mar=c(6,6,1,1), oma=c(0,0,2,0))
plotRule(ma);mtext(text = "(A)", side=3, outer = FALSE, line = 1 ,adj=0)
plotRule(mb);mtext(text = "(B)", side=3, outer = FALSE, line = 1 ,adj=0)
plotRule(mc);mtext(text = "(C)", side=3, outer = FALSE, line = 1 ,adj=0)

dev.off()
#====================================================================
