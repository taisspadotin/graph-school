main <- function()
{
  add <- c() #Vetor que recebe as notas das provas que poderão ser refeitas
  grades <-c() #Vetor que recebe as notas da prova
  
  totalExames <- readline(prompt="Quantas provas são?")
  
  for(i in 1:totalExames){
    ex <- readline(prompt=paste("Digite a nota da prova ",i,":"))
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
        print("Não vai fazer mais nada pra deixar de ser troxa!")
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
  library("DBI");
  library("RPostgreSQL");
  
  pw <- {
    "password"
  }
  
  # loads the PostgreSQL driver
  drv <- dbDriver("PostgreSQL")
  
  con <- dbConnect(drv, dbname = "postgres",
                   host = "localhost", port = 5432,
                   user = "postgres", password = pw)
  rm(pw) # removes the password
  df_postgres <- dbGetQuery(con, "select test, avg(grade) as media from grades group by test ORDER BY test ASC")
  rs <- as.data.frame(df_postgres);
  tot <- nrow(rs)
  
  mediaGeral = c()
  
  for(i in 1:tot){
    for(j in 1:totalExames){
      if(rs$test[i] == j){
        mediaGeral <- c(mediaGeral, rs$media[i])
      }
    }
  }
  
  # close the connection
  #dbClearResult(rs)
  dbDisconnect(con)
  
  exames <- c() #Vetor que vai receber o total de provas
  for(i in 1:totalExames){
    exames <- c(exames, i)
  }
  
  #CRIAÇÃO DO GRÁFICO
  #plot(exames,grades) # o mesmo que o anterior
  plot(exames, grades, lwd  = 2, main="Notas no exame", ylab="Notas", xlab="Provas", type="l",  col="purple", xlim=c(1, as.numeric(totalExames)), ylim=c(1, 10))
  #Criando um grafico que ira sobrepor com o outro
    lines(exames, mediaGeral, lwd  = 2, col="green", type="l", lty  = "dashed")
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
