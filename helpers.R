
# Visualize
visualize <- function(board = mat,
                      yoni_color = "blue",
                      joe_color = "green"){
  plot(1:9, 1:9, type = "n")
  
  points(x = rep(1:9, each =9),
         y = rep(9:1, 9),
         col = adjustcolor(ifelse(board == 1, joe_color, ifelse(board == 2, yoni_color, "grey")), 
                           alpha.f = 0.3),
         pch = ifelse(board == 1, "X", ifelse(board == 2, "O", "")),
         cex = 3)  
  
  text(x = rep(1:9, each =9),
       y = rep(9:1, 9), 
       labels = ifelse(board == 1, "joe", ifelse(board == 2, "yoni", "")),
       col = adjustcolor("black", alpha.f = 0.3))


  
  abline(h = seq(0.5,10.5, 1),
         v = seq(0.5, 10.5, 1),
         lty = 2,
         col = adjustcolor("black", alpha.f = 0.4))
  abline(h = seq(3.5, 10.5, 3),
         v = seq(3.5, 10.5, 3),
         lwd = 2)
}

# Update board
update_board <- function(fun){
  this_move <- fun()
  
  x <- this_move$x
  y <- this_move$y
  
  # update the matrix
  mat[x,y] <- this_move$number
  
  # create simple dataframe of new move
  new_move <- data.frame(number = nrow(moves) + 1,
                         player = as.character(bquote(fun)),
                         x = x,
                         y = y)
  # bind the new_move into the moves dataframe
  moves <- rbind(moves, new_move)
  
  # return_objects
  return_objects <- list()
  return_objects$moves <- moves
  return_objects$mat <- mat
  return(return_objects)
}
