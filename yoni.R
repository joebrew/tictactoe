# Example function for Yoni
yoni <- function(board = mat, 
              last_x = NULL,
              last_y = NULL,
              number = 2){
  
  # Check to see if there have already been 81 moves
  if(nrow(moves) > 80){
    stop("Game over")
  }
  
  # Extract last move from moves
  last_move <- moves[nrow(moves),]
  last_x <- last_move$x
  last_y <- last_move$y
  
  # Make sure both a last_x and last_y are supplied (or neither)
  if(is.null(last_x) != is.null(last_y)){
    stop("Provide a complete previous move")
  }
  
  # Make random first move if no last move
  if(is.null(last_x)){
    # Make move
    x <- sample(1:9, size = 1)
    y <- sample(1:9, size = 1)
  } else {
    
    # Define which area is legal
    x_zone <- last_x %% 3
    x_zone <- ifelse(x_zone < 1, 3, x_zone)
    y_zone <- last_y %% 3
    y_zone <- ifelse(y_zone < 1, 3, y_zone)
    x_zone <- (x_zone*3) - (c(0, 1, 2))
    y_zone <- (y_zone*3) - (c(0, 1, 2))
    
    # Check to see if the zone is already completely occupied
    if(0 %in% board[x_zone, y_zone]){
      
      # Subset the matrix
      zone <- board[x_zone, y_zone]
      
      # Make move
      x <- sample(x_zone, size = 1)
      y <- sample(y_zone, size = 1)
      
      # Remake move if it's already occupied
      while(board[x,y] != 0){
        x <- sample(x_zone, size = 1)
        y <- sample(y_zone, size = 1)
      }
      
    } else {
      # Redefine x zone and y zone to include whole board
      x_zone <- 1:9
      y_zone <- 1:9
      
      # Make move
      x <- sample(x_zone, size = 1)
      y <- sample(y_zone, size = 1)
      
      # Remake move if it's already occupied
      while(board[x,y] != 0){
        x <- sample(x_zone, size = 1)
        y <- sample(y_zone, size = 1)
      }
    }    
  }
  move <- list("x" = x,
               "y" = y,
               "number" = number)
  
  return(move)
}
