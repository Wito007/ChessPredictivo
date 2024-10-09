# Instalamos MLmetrics y lo cargamos
if (!require("MLmetrics", character.only = TRUE)) {
install.packages("MLmetrics")
}
library(MLmetrics)



tb <- as.matrix(read.csv("..\data.csv",header = TRUE,sep = ";"))

Openings <- as.matrix(read.csv("..\data.csv",header = TRUE,sep = ";"))



print(Openings)

GetMatrixWhite <- function(name)
{
  m <- as.matrix(0,)
  for (r in 1:nrow(tb))
  {
    if(tb[r,1] == name)
    {
      m <- tb[r:(r+3), 1:3]
      break
    }
  }
  return(m)
}

GetMatrixBlack <- function(name)
{
  m <- as.matrix(0,)
  for (r in 1:nrow(tb))
  {
    if(tb[r,1] == name)
    {
      m <- tb[(r+3):(r+3), (1+5):3]
      break
    }
  }
  return(m)
}

SumO <- function(M1,M2)
{
  #creamos una lista de cantidades de aperturas con M1
  sums <- M1[1:4, 2:3]
  
  for (rm in 1:nrow(M2))
  {
    Append <- TRUE
    
    #si el valor ya estaba en la lista, lo sumamos
    for (rs in 1:nrow(sums))
    {
      if (M2[rm,2] == sums[rs,1])
      {
        sums[rs,2] <- as.numeric(M2[rm,3]) + as.numeric(sums[rs,2])
        Append <- FALSE
        break
      }
    }
    
    #Si no esta repetido, lo agregamos a sums
    if(Append == TRUE)
    {
      sums <- rbind(sums, M2[rm, 2:3])
    }
  }
  
  
  return(sums)
}


FindScores <- function(sums,eloDiff)
{
  scores <- list()
  
  #Encontramos el total de partidas jugadas con estas aperturas
  totalPlays <- 0
  for (r in 1:nrow(sums))
  {
    totalPlays <- totalPlays + as.numeric(sums[r,2])
  }
  
  for (r in 1:nrow(sums))
  {
    #Definimos la probabilidad de que se juegue esta apertura
    prob <- as.numeric(sums[r,2])/totalPlays

    for (ro in 1:nrow(Openings))
    {
      if(Openings[ro,1] == sums[r,1])
      { 
        #si la ventaja es para blanca, sumamos la diferencia de elo
        if(as.numeric(Openings[ro,2]) > 0)
        {
          scores <- append(scores, (as.numeric(Openings[ro,2]) + eloDiff) * prob)
          break
        }
        else
        {
          scores <- append(scores, (as.numeric(Openings[ro,2]) + eloDiff) * prob)
          break
        }
        
        #falta differenciar si la diferencia de elo es para blancas o negras
        scores <- append(scores, (as.numeric(Openings[ro,2]) - eloDiff) * prob)
        break
      }
      
    }
  }
  return(scores)
}


#GM1 es para blancas, GM2 es para negras
Match <- function(GM1, GM2)
{
  #retiraos las matrices de cada jugador de la base de datos
  M1 <- GetMatrixWhite(GM1)
  M2 <- GetMatrixBlack(GM2)
  
  #creamos una lista de aperturas entre los jugadores
  Sums <- SumO(M1,M2)     
  
  #Se define la diferencia ya reducida por un factor n>1
  EloDiff <- (as.numeric(M1[2,1]) - as.numeric(M2[2,1]))*0.1
  
  #Creamos una lista de los valores ponderados por elo y probabilidad
  Scores <- FindScores(Sums,EloDiff)
  
  #Sumamos la ventaja ponderada de blancas y negras
  White <- 0
  Black <- 0
  for (r in 1:length(Scores))
  {
    if(Scores[r] > 0)
    {
      White <- White + as.numeric(Scores[r])
    }
    else if(Scores[r] < 0)
    {
      Black <- Black + as.numeric(Scores[r])
    }
  }
  
  Total <- abs(White) + abs(Black)
  
  WhiteChance <- (White/Total)
  BlackChance <- abs(Black/Total)
  
  print(paste0("Chances de blanco: ",WhiteChance *100,"%"))
  print(paste0("Chances de negro: ",BlackChance *100,"%"))
  
  return(c(WhiteChance,BlackChance))
}


y_true <- c(1, 0, 1, 1, 0)
y_pred <- c(0.1, 0.9, 0.9, 0.9, 0.9)

log_loss <- LogLoss(y_pred, y_true)
print(log_loss)


