#==========================================================
#Kokko capitulo 8 - simulacoes baseadas em individuos
#==========================================================

#exemplo da evolucao da migracao em metapopulacoes

#Nesse script, eu deixei soh a funcao que roda o ibm de
#dispersao. No outro script ("Kokko Cap 8 - parte 2.r")
#eu reproduzi a figura 8.2 e fiz mais uma pequena
#exploracao do espaco de parametros

#O que tem no mundo?
#sitios (sites) e individuos

#quais as caracteristicas dos individuos?
#x - propensao de dispersao - probabilidade de sair do sitio natal
#site - localizacao atual - numero inteiro

#A simulacao tem N sitios (numerados de 1 a N) e, inicialmente,
#M individuos distribuidos aleatoriamente entre eles
#(na versao da Kokko, tem um individuo por sitio)

#A populacao inicial tem x variando uniformemente entre
#zero e um

#processos que tem que ocorrer
#destruicao de sitios
#movimentacao dos individuos
#competicao por espaco (ou outro recurso)
#reproducao
#mutacao


#==========================================================
#funcao que roda a simulacao
dispersal = function(N = 100, #numero de sitios
                     M = 300, #numero de individuos
                     B = 2, #numero maximo de individuos por sitio
                     b = 2, #natalidade
                     p = 0.01, #probabilidade do sitio ser arrasado
                     q = 0.01, #taxa de mutacao
                     m = 0.1,  #mortalidade dos dispersantes
                     G = 1000) #numero de geracoes pra simular
{  
  
  #debug
  #N = 100; M = 300; B = 2; b = 2; p = q = 0.01; m = 0.1; G = 100
  
  #criando a populacao inicial, com localizacao
  #e o valor de x. Eu modifiquei aqui para que cada individuo caisse
  #aleatoriamente em um sitio qualquer
  pop = data.frame(site = sample(1:N, M, replace=TRUE), x = runif(N, 0, 1))
  
  #matriz de registro de informacoes
  #media e desvio de x, tamanho populacional total, numero de sitios ocupados
  rec = matrix(NA, ncol = 4, nrow = G)
  colnames(rec) = c("xmean", "xsd", "popsize", "occSites")
  
  #preenchendo as colunas 3 e 4 com zeros
  rec[,3] = 0
  rec[,4] = 0
  
  for(g in 1:G) #percorre as G geracoes
  {
    #g=1
    #armazena o estado atual da populacao na matriz rec
    rec[g, 1] = mean(pop$x)
    rec[g, 2] = sd(pop$x)
    rec[g, 3] = nrow(pop)
    
    ###perturbacao (mortalidade geral em alguns sitios)
    
    #o rbinom sorteia um valor zero ou um para cada
    #sitio, com probabilidade de ser 1 igual a p.
    destruction = rbinom(n = N, size = 1, prob = p)
    
    #agora, com um which, conseguimos recuperar quem tem que morrer!
    destroyed = which(destruction==1)
  
    #usando indexacao, podemos retirar da populacao quem tiver
    #que morrer
    pop = pop[!(pop$site %in% destroyed),]
    ###fim da perturbacao
    
    ###verificacao de sobrou alguem vivo
    if(nrow(pop) == 0)
      break
    
    ###competicao
    #soh podem haver B individuos por mancha
    
    #calculando o tamanho populacional por mancha
    popSizes = tabulate(pop$site, nbins = N)
    
    #verifica de algum tamanho populacional eh maior que B
    if(any(popSizes>B))
    {
      #vetor onde vamos guardar quem vive e quem morre
      #inicialmente, todo mundo sobrevive
      survives = rep(1, nrow(pop))
      
      for(i in 1:N)#percorre as manchas de habitat
      {
        if(popSizes[i]>B)
        {
          positions = which(pop$site == i)#posicoes de quem mora no sitio i
          
          #aqui a gente cria um vetor com uma quantidade de 1s igual a B
          #e uma quantidade de zeros igual ao numero excedente de individuos
          #ai a gente aleatoriza o vetor com sample, e guarda nas posicoes adequadas
          #do vetor sobrevive
          survives[positions] = sample(c(rep(1, B), rep(0, popSizes[i]-B)))
        }
      }
      #mantem no data.frame soh quem foi sorteado pra sobreviver
      pop = pop[survives == 1,]
    }
    
    #recalculando os tamanhos populacionais
    popSizes = tabulate(pop$site, nbins = N)
    
    #esse comando pode ser usado pra conferir se a competicao funcionou
    #table(popSizes)
    
    #guardando o numero de sitios ocupados apos a competicao
    rec[g, 4] = sum(popSizes>0)
    
    ###fim da competicao - deve ter um jeito mais esperto de fazer isso, mas por enquanto vai ser esse forzao mesmo
    
    ###nascimento da nova geracao (e mutacao)
    
    #contando o numero de individuos
    nInds = nrow(pop)
    
    #criando um novo data.frame newpop em que cada individuo vivo eh repetido b vezes
    newpop = pop[rep(1:nInds, each = b),]
    newPopSize = nrow(newpop)
    
    #hora da mutacao
    
    #sorteia onde vao ocorrer as mutacoes
    mutation = rbinom(n = nrow(newpop), size = 1, prob = q)
    
    if(any(mutation == 1)) #if any mutation happened
    {
      #troca os valores dos mutantes por um outro valor aleatorio
      nMutants = sum(mutation)
      newpop$x[mutation == 1] = runif(sum(mutation), 0, 1) 
    }
    
    ###fim do processo de nascimento e mutacao
    
    ###dispersao e mortalidade
    #sorteia quem vai se dispersar
    disperses = rbinom(n = newPopSize, size = 1, prob = newpop$x)
    
    #sorteia um novo lar para todos com 1 no vetor disperses
    nDisp = sum(disperses)
    newpop$site[disperses == 1] = sample(1:N, size = nDisp, replace = TRUE)
    
    #mortalidade entre migrantes
    dead = rbinom(n = newPopSize, size = 1,
                  prob = ifelse(disperses == 1, m, 0))
    #na verdade, o que eu fiz aqui foi sortear uma probabilidade
    #de morte para cada individuo. Mas quem nao dispersou tem mortalidade
    #igual a zero, e os outros tem igual a m. Soh pra facilitar as indexacoes
    
    newpop = newpop[dead == 0,]
    
    ###fim do processo de migracao
    
    ###morte por reescrivinhamento
    pop = newpop
  } #fim do for(g in 1:G)
  
  #retorna pro usuario a matriz rec com todas
  #as variaveis que a gente registrou
  return(data.frame(rec))
}
#compilando a funcao com o pacote compiler.
#teoricamente, isso pode acelerar um pouco as coisas
library(compiler)
dispersal = cmpfun(dispersal)
#==========================================================