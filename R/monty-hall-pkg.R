#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title Select a Door
#' @description Randomly selects one of the three doors for the contestant.
#' @details This function simulates the contestant's initial random choice of one of the three doors.
#' @return A numeric value representing the chosen door (1, 2, or 3).
#' @examples
#' select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title Open a Goat Door
#' @description Opens a door with a goat behind it, excluding the chosen door unless it contains the car.
#' @details If the contestant's chosen door contains the car, a random goat door is opened. Otherwise, the only possible goat door is opened.
#' @param game A character vector representing the game setup (e.g., `c("goat", "car", "goat")`).
#' @param a.pick A numeric value indicating the contestant's selected door.
#' @return A numeric value representing the door opened by the host (1, 2, or 3).
#' @examples
#' game <- create_game()
#' open_goat_door(game, 1)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title Change Door
#' @description Changes the contestant's door choice based on their decision to stay or switch.
#' @details If the contestant decides to stay, their original choice is returned. If they switch, the unopened door is returned.
#' @param stay Logical; `TRUE` if the contestant stays with their choice, `FALSE` if they switch.
#' @param opened.door A numeric value representing the door opened by the host.
#' @param a.pick A numeric value representing the contestant's current door choice.
#' @return A numeric value representing the contestant's final door choice (1, 2, or 3).
#' @examples
#' change_door(stay = TRUE, opened.door = 2, a.pick = 1)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title Determine Winner
#' @description Determines whether the contestant wins or loses based on their final door choice.
#' @details Compares the contestant's final door choice with the game setup to determine the outcome.
#' @param final.pick A numeric value representing the contestant's final door choice.
#' @param game A character vector representing the game setup (e.g., `c("goat", "car", "goat")`).
#' @return A character string: `"WIN"` if the contestant chooses the car, `"LOSE"` otherwise.
#' @examples
#' game <- create_game()
#' determine_winner(1, game)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title Play a Single Game
#' @description Simulates one complete game of the Monty Hall problem using both "stay" and "switch" strategies.
#' @details The game is played with both strategies ("stay" and "switch"), and the outcomes are recorded.
#' @param n None.
#' @return A data frame with two rows: the strategy ("stay" or "switch") and the outcome ("WIN" or "LOSE").
#' @examples
#' play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title Play Multiple Games
#' @description Simulates multiple games of the Monty Hall problem and summarizes the results.
#' @details Repeats the Monty Hall game `n` times, calculates proportions of wins for each strategy, and returns a summary.
#' @param n An integer specifying the number of games to simulate (default is 100).
#' @return A data frame containing the outcomes of all games played.
#' @examples
#' play_n_games(100)
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}




