add_points <- function(sub_board, number){
  total_points <- 0
  # horizontal
  if(all(sub_board[1,] == number)){total_points <- total_points + 1}
 if( all(sub_board[2,] == number)){total_points <- total_points + 1}
 if( all(sub_board[3,] == number)){total_points <- total_points + 1}
  
  # vertical
 if( all(sub_board[,1] == number)){total_points <- total_points + 1}
 if( all(sub_board[,2] == number)){total_points <- total_points + 1}
 if( all(sub_board[,3] == number)){total_points <- total_points + 1}
  
  # diagonal
 if( all(sub_board[1:3, 1:3] == number)){total_points <- total_points + 1}
 if( all(sub_board[1:3, 3:1] == number)){total_points <- total_points + 1}
 
 # return
 return(total_points)
}

score <- function(board = mat){
  output <- list()
  output$yoni <- 0
  output$joe <- 0 
  for (i in list(1:3, 4:6, 7:9)){
    for (j in list(1:3, 4:6, 7:9)){
      
      # Define sub_board
      sb <- board[i,j]
      
      # Calculate and update points
      points_joe <- add_points(sub_board = sb, number = 1)
      output$joe <- output$joe + points_joe
      
      points_yoni <- add_points(sub_board = sb, number = 2)
      output$yoni <- output$yoni + points_yoni

      # Label the points on the visual
      if(!is.null(dev.list())){
        text(x = j[2],
             y = 10 - i[2],
             labels = paste("Joe:", points_joe, "\n",
                            "Yoni:", points_yoni),
             cex = 2.5,
             col = adjustcolor("darkred", alpha.f = 0.3))  
        
        
        
        points(x = j[2],
               y = 10 - i[2],
               cex = 10,
               pch = ifelse(points_joe > points_yoni, "X",
                            ifelse(points_yoni > points_joe, "O",
                                   24)),
               col = adjustcolor(ifelse(points_joe > points_yoni, "darkgreen",
                            ifelse(points_yoni > points_joe, "darkblue",
                                   "darkorange")), alpha.f = 0.6)
               )
        
      }
    }

  }
  
  # Outside of loop, give title (can't get this to work)
  if(output$joe == output$yoni){
    my_title <- paste("It's a tie! Joe =", output$joe, "Yoni =", output$yoni) 
  } else if(output$joe > output$yoni){
    my_title <- paste("Joe wins,", output$joe, "to", output$yoni)
  } else if(output$joe < output$yoni){
    my_title <- paste("Yoni wins,", output$yoni, "to", output$joe)
  } else 
    
    if(!is.null(dev.list())){
      title(main = my_title)    
    }
  
   return(output)

}
