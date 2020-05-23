main <- function()
{
  grades <- c(7, 8, 5, 4)
  add <- c()
  
  for(i in 1:length(grades)) 
  {
    if((grades[i]) < 6){
      print(paste("A nota ",i," -> ",grades[i], " pode ser refeita"))
      #print(readinteger())
      n <- readline(prompt="Deseja refazer? y/n ")
      if(n == "y"){
        print("simmmm")
        add <- c(add, grades[i])
      }
      else if(n == "n"){
        print("tudo bem, n faz")
      } 
      else{
        print("Não vai fazer mais nada pra deixar de ser bobo!")
      }
    }
  }
  print("As provas que podem ser refeitas, são as com as seguintes notas:")
  print(add)
  all_remake <- mean(add)
  print(paste("A media das provas que serão refeitas é", all_remake))
  
  all <- mean(grades)
  print(paste("A media do aluno é", all))
  resp <- readline(prompt="Deseja ver o gráfico? y/n ")
  if(resp == 'y')
  {
    print(graph(grades))
  }
 
}

graph <- function(grades){
  library(ggplot2)
  library(dplyr)
  
  exames <- c(1 ,2 , 3, 4)
  exames.cate <- rep(c("pequeno", "grande"), each=1)
  
  #Média da sala de aula
  averageCR <- c(5, 4, 4, 8)
  
  #CRIAÇÃO DO GRÁFICO
  #plot(exames,grades) # o mesmo que o anterior
  plot(exames, grades, lwd  = 2, main="Notas no exame", ylab="Notas", xlab="Provas", type="l",  col="purple", xlim=c(1, 4), ylim=c(1, 10))
  #Criando um grafico que ira sobrepor com o outro
  lines(exames, averageCR, lwd  = 2, col="green", type="l", lty  = "dashed")
  legend("topright",
         c("Média do aluno","Média da sala"),
         fill=c("purple","green")
  )
  # cex  = 1 -> Controla o tamanho da bolinha
  #lwd -> controla o width da linha
  #Type:
    #"p" - points
    #"l" - lines
    #"b" - both points and lines
    #"c" - empty points joined by lines
    #"o" - overplotted points and lines
    #"s" and "S" - stair steps
    #"h" - histogram-like vertical lines
    #"n" - does not produce any points or lines
}

print(main())
