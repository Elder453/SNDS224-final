##########################################################
# `simulate_pig` function
#
# Purpose:
#   Simulates a single-player game of Pig using a simple
#   HEURISTIC strategy to decide whether to roll or hold.
#
# Inputs:
#   hold_threshold: The turn total at which the player holds
#                   (default is 20).
#   VERBOSE: Logical flag to print detailed turn-by-turn
#            game progress (default is FALSE).
#
# Outputs:
#   The total number of turns taken to finish the game.
#
# Notes:
#   - A roll of 1 resets the turn total and ends the turn.
#   - The player holds if they can win or if the turn total
#     meets or exceeds the `hold_threshold`.
##########################################################

# Constants
TARGET_SCORE <- 100
DIE_SIDES <- 6
ROLL_PROBS <- rep(1/DIE_SIDES, DIE_SIDES)
ROLL_VALS <- 1:DIE_SIDES

simulate_heuristic_pig <- function(
    hold_threshold=20, 
    VERBOSE=FALSE) 
  {
  curr_score <- 0  # Overall score
  turn_total <- 0  # Points accumulated in the current turn
  turn_number <- 1 # Track the turn number
  
  while (curr_score < TARGET_SCORE) {
    if (VERBOSE) {
      cat("\n========== Turn", turn_number, "==========\n")
      cat("Current Overall Score:", curr_score, "\n")
      cat("Starting Turn Total:", turn_total, "\n")
    }
    
    # The player must roll on the first move of the turn
    roll <- sample(1:6, 1)
    if (VERBOSE) {cat("First Roll of the Turn:", roll, "\n")}
    
    if (roll == 1) {
      # If the first roll is 1, the turn ends immediately
      turn_total <- 0
      if (VERBOSE) {
        cat("Rolled a 1! Turn total lost. Overall Score remains at",
            curr_score, "\n\n")
      }
      turn_number <- turn_number + 1
      next
    } else {
      # Add the roll to the turn total
      turn_total <- roll
    }
    
    # After the first roll, follow the simple strategy
    repeat {
      if (curr_score + turn_total >= TARGET_SCORE || turn_total >= hold_threshold) {
        # Hold if the player can win or the turn total is at least 20
        curr_score <- curr_score + turn_total
        turn_total <- 0
        if (VERBOSE) {cat("Held. New Overall Score:", curr_score, "\n\n")}
        break  # End the turn
      } else {
        # Roll the die
        roll <- sample(1:6, 1)
        if (VERBOSE) {cat("Rolled a", roll, "\n")}
        if (roll == 1) {
          # Lose turn total
          turn_total <- 0
          if (VERBOSE) {
            cat("Rolled a 1! Turn total lost. Overall Score remains at",
                curr_score, "\n\n")
          }
          break  # End the turn
        } else {
          # Add roll to turn total
          turn_total <- turn_total + roll
          if (VERBOSE) {cat("Turn Total after rolling:", turn_total, "\n")}
        }
      }
    }
    
    # Move to the next turn
    turn_number <- turn_number + 1
  }
  
  if (VERBOSE) {
    cat("\n========== Game Over ==========\n")
    cat("Final Overall Score:", curr_score)
  }
  return(turn_number - 1)  # Return the number of turns
}

