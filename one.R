main <- function()
{
  #grades <- c(7, 8, 5, 4)
  add <- c() #Vetor que recebe as notas das provas que poderão ser refeitas
  grades <-c() #Vetor que recebe as notas da prova
  
  totalExames <- readline(prompt="Quantas provas são?")
  
  for(i in 1:totalExames){
    ex <- readline(prompt="Digite a nota da prova :")
    grades <- c(grades, ex)
  }
  
  
  for(i in 1:length(grades)) 
  {
    if((grades[i]) < 6){
      print(paste("A nota ",i," -> ",grades[i], " pode ser refeita"))
      
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
  all_remake <- mean(as.numeric(add))
  print(paste("A media das provas que serão refeitas é", all_remake))
  
  all <- mean(as.numeric(grades))
  print(paste("A media do aluno é", all))
  
  resp <- readline(prompt="Deseja ver o gráfico? y/n ")
  if(resp == 'y')
  {
    print(graph(grades, totalExames))
  }
  
 
}

graph <- function(grades, totalExames){
  # install.packages("RPostgreSQL")
  require("RPostgreSQL")
  
  # create a connection
  # save the password that we can "hide" it as best as we can by collapsing it
  pw <- {
    "senha"
  }
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  # creates a connection to the postgres database
  # note that "con" will be used later in each connection to the database
  con <- dbConnect(drv, dbname = "postgres",
                   host = "localhost", port = 5432,
                   user = "postgres", password = pw)
  rm(pw) # removes the password
  
  # check for the cartable
  dbExistsTable(con, "grades")
  # TRUE
  
  
  # query the data from postgreSQL 
  df_postgres <- dbGetQuery(con, "SELECT * from grades")
  print(df_postgres)
  
  # close the connection
  #dbDisconnect(con)
  #dbUnloadDriver(drv)
  
  exames <- c() #Vetor que vai receber o total de provas
  averageCR <- c()
  for(i in 1:totalExames){
    exames <- c(exames, i)
    averageCR <- c(averageCR, i+1)
  }
  
  #exames.cate <- rep(c("pequeno", "grande"), each=1)
  
  #Média da sala de aula
  
  
  #CRIAÇÃO DO GRÁFICO
  #plot(exames,grades) # o mesmo que o anterior
  plot(exames, grades, lwd  = 2, main="Notas no exame", ylab="Notas", xlab="Provas", type="l",  col="purple", xlim=c(1, as.numeric(totalExames)), ylim=c(1, 10))
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
