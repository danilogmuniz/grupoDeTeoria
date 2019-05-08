#==========================================================
#RODANDO!
#==========================================================
#rm(list=ls()) #limpando a area de trabalho

#carregando a funcao
source("Kokko Cap 8 - ibm de dispersao.r")

#rodando tres simulacoes
teste1 = dispersal(N = 100, M = 100, B = 1)
teste2 = dispersal(N = 100, M = 100, B = 2)
teste3 = dispersal(N = 100, M = 100, B = 3)

#desenhando a figura 8.2

#png(filename = "kokko.cap.8.png", width = 20, height=15, units = "cm",
#    res=450)
par(mfrow=c(3,3), las=1, bty="l", cex.lab=1.2, mar=c(6,6,1,1))
plot(teste1$popsize, type = "l", ylim=c(0,600),ylab="Population size", xlab="")
plot(teste2$popsize, type = "l", ylim=c(0,600),ylab="", xlab="Generations")
plot(teste3$popsize, type = "l", ylim=c(0,600),ylab="", xlab="")

plot(teste1$occSites, type = "l", ylim=c(0,100),ylab="Occupied sites", xlab="")
plot(teste2$occSites, type = "l", ylim=c(0,100),ylab="", xlab="Generations")
plot(teste3$occSites, type = "l", ylim=c(0,100),ylab="", xlab="")

plot(teste1$xmean, type = "l", ylim=c(0,1),ylab="Mean dispersal\npropensity", xlab="")
plot(teste2$xmean, type = "l", ylim=c(0,1),ylab="", xlab="Generations")
plot(teste3$xmean, type = "l", ylim=c(0,1),ylab="", xlab="")
#dev.off()


#==========================================================
#tentando fazer uma versao melhorada da figura 8.3h

#valores de parametros que vamos rodar
N = 20; B = 5; p = 0.05; q = 0.01; b = 5
m = seq(from = 0.1, to=0.9, by=0.1)

#numero de repeticoes que vamos rodar de cada combinacao
reps = 20

#Vamos fazer um forzinho, pra rodar varias simulacoes com cada valor
#e desenhar um boxplot bonito no final

#montando um data.frame com os valores de parametros
#que vamos rodar

combs = expand.grid(N = N, B = B, p = p, q=q, b=b, m=m)

#montando um data.frame pra guardar as informacoes de todas as simulacoes
simdat = combs[rep(1:nrow(combs), reps),]

#adicionando colunas onde vamos armazenar os dados de saida
#(vamos guardar a media das ultimas dez geracoes, so pra
#diminuir o efeito das variacoes aleatorias que continuam
#eternamente nas simulacoes)
simdat$xmean = NA
simdat$xsd = NA
simdat$popsize = NA
simdat$occSites = NA

head(simdat)
#ok, a gambiarra funcionou!

inicio = proc.time() #guardando o horario de inicio
for(s in 1:nrow(simdat))
{
  #rodando uma simulacao com os valores de parametros de simdat linha s
  #s = 1
  sims = dispersal(N = simdat$N[s],
                   M = simdat$N[s],#vamos rodar sempre M = N
                   B = simdat$B[s],
                   p = simdat$p[s],
                   q = simdat$q[s],
                   b = simdat$b[s],
                   m = simdat$m[s], G = 500)
  
  #calculando a media das ultimas dez geracoes
  means = apply(sims[491:500,], 2, mean)
  
  #armazenando no data.frame simdat
  simdat[s, 7:10] = means
  
  #aviso amigável pra gente saber que o PC nao travou
  cat("simulacao", s, "concluida. Hold on.\n")
}
demora = proc.time() - inicio #calculando a demora
cat("Demorou apenas", demora[3]/60, "minutinhos.\n")
#na minha maquina foi rapidao!! 3.22 minutos

tail(simdat)
par(mfrow=c(1,1))
boxplot(xmean~m, data=simdat, col="darkorange", range=0)

#ok, aparece uma sutil curva em U
#mas, de modo geral, conforme a mortalidade na dispersao
#aumenta, a dispersao diminui.

#mas o tamanho populacional total, ainda cai muito como m
boxplot(popsize~m, data=simdat, col="dodgerblue", range=0)

#assim como o numero de sitios ocupados
boxplot(occSites~m, data=simdat, col="orchid", range=0)


#==========================================================