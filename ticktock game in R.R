# first lets build game mechanics

random_move=function(board){
  decision=NA
  empty=F
  for (j in 1:90) {
    if (empty==F) {
    available_moves=which(board==0)
    decision=sample(available_moves, 1)
    if (board[decision]==0) {
      empty=T
    }
    }
  }
  if (empty==F) {
    decision=NA
  }
  return(decision)
}
make_move=function(board, pl, next_move){
  if (!is.na(next_move)) {
  board[next_move]=pl
  }
  return(board)
}

evaluate_win <- function(board){
  board_mat <- matrix(board, ncol = 3, byrow = T)
  win_conditions <- list(
    board_mat[1,], board_mat[2,], board_mat[3,], 
    board_mat[,1], board_mat[,2], board_mat[,3], 
    diag(board_mat), diag(board_mat[,c(3:1)])    
  )
  winner <- NULL
  if(list(c(1,1,1)) %in% win_conditions){       
    winner <- 1
  }else if (list(c(2,2,2)) %in% win_conditions){  
    winner <- 2
  }else if (0 %in% board_mat){              
    winner <- NULL
  }else {
    winner <- 0
  }
  return(winner)
}

### game

game <- function(){
  
  # Init Board
  board <-  c(0,0,0,
              0,0,0,
              0,0,0
              )
  winner = NULL
  traindf=data.frame()
  moves=data.frame()
  counter=1
  while(is.null(winner)){
    #Player 2:
    pl=2
    board2=board
    board2[board2==0]="."
    board2[board2==1]="O"
    board2[board2==2]="X"
    state1=paste(board2,collapse="")
    next_move <- random_move(board)
    board <- make_move(board, pl, next_move)
    action=paste("A",next_move,collapse="")
    winner <- evaluate_win(board)
    r=0
    if (!is.null(winner)) {
      if (winner==2) {
        r=1
      }
      if (winner==1) {
        r=-1
      }
    }
    
    traindf[1,counter]=next_move
    counter=counter+1
    if(is.null(winner)){
      #Player 1:
      pl=1
      available_moves=which(board==0)
      next_move <- random_move(board)
      board <- make_move(board, pl, next_move)
      winner <- evaluate_win(board)
      traindf[1,counter]=next_move
      counter=counter+1
      board2=board
      board2[board2==0]="."
      board2[board2==1]="O"
      board2[board2==2]="X"
      state2=paste(board2,collapse="")
      r=0
      if (!is.null(winner)) {
        if (winner==2) {
          r=1
        }
        if (winner==1) {
          r=-1
        }
      }
      
    }
    moves=rbind(moves,data.frame(state1,action,state2,r))
  }
  if(length(traindf)<9){
    traindf[1,(length(traindf)+1):9]=0
  }
  traindf[1,10]=winner
  names(traindf)=c("P1,1","P2,1","P1,2","P2,2","P1,3","P2,3","P1,4","P2,4","P1,5","result")
  return(moves)
}

#### random vs AI

#use game to generate data set
trainingdata=data.frame()
for (i in 1:50000) {
  if (i==1) {
    trainingdata=game()
  }else{
    debug=game()
  trainingdata=rbind(trainingdata,debug)
  }
  print(i)
}
library(dplyr)
trainingdata=distinct(trainingdata)

# ## training neural net

# to install needed libraries

#  install.packages("devtools")
# 
# # Option 1: download and install latest version from GitHub
# devtools::install_github("nproellochs/ReinforcementLearning")
# 
# # Option 2: install directly from bundled archive
# devtoos::install_local("ReinforcementLearning_1.0.0.tar.gz")

library(ReinforcementLearning)
model <- ReinforcementLearning(data = trainingdata, 
                               s = "state1", 
                               a = "action", 
                               r = "r", 
                               s_new = "state2", 
                               iter = 1)
##game with ai
gameAI <- function(){
  
  # Init Board
  board <-  c(0,0,0,
              0,0,0,
              0,0,0
  )
  winner = NULL
  traindf=data.frame()
  moves=data.frame()
  counter=1
  while(is.null(winner)){
    #Player 2:
    pl=2
    board2=board
    board2[board2==0]="."
    board2[board2==1]="O"
    board2[board2==2]="X"
    state1=paste(board2,collapse="")
    empty=F
    if (state1 %in% trainingdata[,1]) {
      next_move <-  as.numeric(strsplit(predict(model,state1)," ")[[1]][2]) 
      if (board[next_move]!=0) {
        next_move <- random_move(board)
      }
    }else{
      next_move <- random_move(board)
    }
    board <- make_move(board, pl, next_move)
    action=paste("A",next_move,collapse="")
    winner <- evaluate_win(board)
    r=0
    if (!is.null(winner)) {
      if (winner==2) {
        r=1
      }
      if (winner==1) {
        r=-1
      }
    }
    
    traindf[1,counter]=next_move
    counter=counter+1
    if(is.null(winner)){
      #Player 1:
      pl=1
      board2=board
      board2[board2==0]="."
      board2[board2==1]="O"
      board2[board2==2]="X"
      available_moves=which(board==0)
      next_move <- random_move(board)
      print(matrix(board2, ncol = 3, byrow = T))
      next_move <- as.numeric(readline("select a place"))
      board <- make_move(board, pl, next_move)
      winner <- evaluate_win(board)
      traindf[1,counter]=next_move
      counter=counter+1
      board2=board
      board2[board2==0]="."
      board2[board2==1]="O"
      board2[board2==2]="X"
      state2=paste(board2,collapse="")
      r=0
      if (!is.null(winner)) {
        if (winner==2) {
          r=1
        }
        if (winner==1) {
          r=-1
        }
      }
      
    }
    moves=rbind(moves,data.frame(state1,action,state2,r))
  }
  board2=board
  board2[board2==0]="."
  board2[board2==1]="O"
  board2[board2==2]="X"
  print(matrix(board2, ncol = 3, byrow = T))
  print(winner)
  if(length(traindf)<9){
    traindf[1,(length(traindf)+1):9]=0
  }
  traindf[1,10]=winner
  names(traindf)=c("P1,1","P2,1","P1,2","P2,2","P1,3","P2,3","P1,4","P2,4","P1,5","result")
  return(moves)
}
## Start game
for (i in 1:3) {
  t=gameAI()
  trainingdata=rbind(trainingdata,t)
  model <- ReinforcementLearning(data = trainingdata, 
                                 s = "state1", 
                                 a = "action", 
                                 r = "r", 
                                 s_new = "state2", 
                                 iter = 1)
}



