
# Swipe the "board"
mat <- matrix(data = 0, 
              nrow = 9,
              ncol = 9)

# Swipe the record of all moves
moves <- data.frame(number = NA,
                    player = NA,
                    x = NA,
                    y = NA)
moves <- moves[-1,]

first <- sample(c("yoni", "joe"), size = 1)

if(first == "joe"){
  functions <- list(joe, yoni)
} else {
  functions <- list(yoni, joe)
}

x11()
#visualize()
while(length(mat[which(mat == 0)]) > 0 & nrow(moves) < 81){
  new_move <- update_board(functions[[1]])
  mat <- new_move$mat
  moves <- new_move$moves
  visualize() ; Sys.sleep(0.01)
  if(nrow(moves) < 81){
    new_move <- update_board(functions[[2]])
    mat <- new_move$mat
    moves <- new_move$moves
    visualize(); Sys.sleep(0.01)
  }
  
}

