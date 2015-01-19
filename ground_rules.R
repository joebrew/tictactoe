# wd
setwd("tictactoe")

# Read in helper functions
source("helpers.R"); source("score.R")

# Source Yoni and Joe's functions
source("yoni.R") ; source("joe.R")

# Play a game
source("play.R")

# Tally up the score
score()

# Play 100 times
results <- data.frame(time = 1:100,
                      yoni = NA,
                      joe = NA)
for (i in 1:100){
  source("play.R")
  results$yoni[i] <- score()$yoni
  results$joe[i] <- score()$jo
}

