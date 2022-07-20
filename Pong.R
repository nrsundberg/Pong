library(stringr)
library(dplyr)
players <- data.frame()
player_number <- 1

new_game <- function() {
  repeat {
    if (player_number == 1) {
      name <- readline(prompt = "Enter Name: ")
      players <<- rbind(c(
        (str_split(as.character(name), pattern = " ")[[1]][1]),
        (str_split(as.character(name), pattern = " ")[[1]][2]),
        player_number,
        0))
      players <<- as.data.frame(players)
      player_number <- player_number + 1
    } else {
      name <- readline(prompt = "Enter Name (type 'done' to stop): ")
      if (name == "done") {
        break
      }
      players <<- rbind(players,c(
        (str_split(as.character(name), pattern = " ")[[1]][1]),
        (str_split(as.character(name), pattern = " ")[[1]][2]),
        player_number,
        0))
      player_number <- player_number + 1
      colnames(players) <<-
        c("First_Name", "Last_Name", "Player_Number", "Full_Name")
      players <<- as.data.frame(players)
    }
  }
}

new_player <- function(){
  repeat {
    name <- readline(prompt = "Enter Name (type 'done' to stop): ")
    if (name == "done") {
      break
    }
    players <<- rbind(players,c(
      (str_split(as.character(name), pattern = " ")[[1]][1]),
      (str_split(as.character(name), pattern = " ")[[1]][2]),
      (nrow(players) + 1),
      NA))
    colnames(players) <<-
      c("First_Name", "Last_Name", "Player_Number", "Full_Name")
    players <<- as.data.frame(players)
    players <<- players %>% 
      mutate(Full_Name = str_c(First_Name, Last_Name, sep = " "))
  }
}

bracket_create <- function() {
  num_games <<- nrow(players) - 1
  num_rounds <<- if (nrow(players) %% 2 == 0) {
    round(
      log(nrow(players), 2),
      digits = 0)
  } else {
    round(
      log(nrow(players) + 1, 2),
      digits = 0)
  }
  bracket <<- data.frame(matrix(NA,
                                nrow = ceiling(nrow(players)/2),
                                ncol = num_rounds * 3))
  colnames(bracket) <<- rep(c("Player 1", "Player 2", "Winner"), num_rounds)
  players <<- players %>% 
    mutate(Full_Name = str_c(First_Name, Last_Name, sep = " "))
  bracket[,1] <<- sample(players$Full_Name, ceiling(nrow(players)/2), replace = FALSE)
  bracket[1: (length(players$Full_Name) - length(bracket[,1])), 2] <<- players$Full_Name[! players$Full_Name %in% bracket[,1]]
}

winner <- function(round, game) {
  winner <- readline("What player won? ")
  round <- as.numeric(round)
  game <- as.numeric(game)
  winner <- as.numeric(winner)
  if (round == 1){
    bracket[game, round * 3] <<- bracket[game, winner] 
  } else {
    if (winner == 1) {
      bracket[game, round * 3] <<- bracket[game, round * 3 - 2] 
    } else {
      bracket[game, round * 3] <<- bracket[game, round * 3 - 1] 
    }
  }
}

retain_bracket <- function() {
  date <- readline("Date? ")
  game <- readline("Bracket? ")
  write.csv(bracket, paste(str_c("C:/Users/Noah/Documents/Ping Pong/Past Brackets/", 
                                 paste(str_c(date, game, sep = "_")), ".csv")))
}

next_round <- function(new_round) {
  new_round <- as.numeric(new_round)
  length_count <- length(bracket[which(bracket[, (new_round - 1) * 3] != is.na(bracket[, (new_round - 1) * 3])), new_round *3])
  ceiling_calc <- ceiling(length_count / 2)
  round_calc <- round(length_count / 2, digits = 0)
  old_round_column <- (new_round - 1) * 3
  new_round_P1 <- new_round * 3 - 2
  new_round_P2 <- new_round * 3 - 1
  bracket[1:ceiling_calc, new_round_P1] <<- bracket[1:ceiling_calc, old_round_column]
  bracket[1:round_calc, new_round_P2] <<- bracket[(ceiling_calc + 1):(ceiling_calc + round_calc), old_round_column]
}

play_tourney <- function() {
  new_game()
  bracket_create()
  View(bracket)
  game <<- 1
  round <<- 1
  repeat {
    repeat {
      winner(round, game)
      game <<- game + 1
      new_round <- readline("New round y or n? ")
      if (new_round == 'y') {
        break
      }
    }
    round <<- round + 1
    next_round(round)
    game <<- 1
    if (round == num_rounds) {
      break
    }
  }
  winner(round, game)
  retain_bracket()
}
