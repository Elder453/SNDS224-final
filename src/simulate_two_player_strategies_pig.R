##########################################################
# `simulate_optimal_vs_optimal_pig` function
#
# Purpose:
#   Simulates a two-player game of Pig where both players 
#   use an OPTIMAL STRATEGY defined by a given policy matrix.
#
# Inputs:
#   V: A 3D array representing the value function for all 
#      game states (TurnTotal, PlayerScore, OpponentScore).
#   policy: A 3D array representing the optimal action 
#           (Roll or Hold) for all game states.
#   VERBOSE: Logical flag to print detailed game progress, 
#            including each player's turn actions and outcomes 
#            (default is FALSE).
#
# Outputs:
#   The winner of the game (1 for Player 1, 2 for Player 2).
#
# Details:
#   - Players take turns rolling a die or holding, based on the 
#     policy derived from the value function.
#   - Rolling a 1 resets the current turn total and ends the turn.
#   - Holding adds the current turn total to the player's score and 
#     ends the turn.
#   - The game ends when one player's score reaches or exceeds the 
#     target score (default is 100).
#
# Notes:
#   - The policy and value function must be precomputed and provided 
#     as inputs to the function.
#   - This function assumes the game mechanics of Pig, including a 
#     single six-sided die, are fixed.
##########################################################

simulate_optimal_vs_optimal_pig <- function(
    V, 
    policy,
    T_MAX=100,        # maximum turn score (to limit state space)
    TARGET_SCORE=100, # score required to win
    DIE_SIDES=6,      # number of sides on the die
    VERBOSE=FALSE    # print progress at each iteration
){
  # Initialize game state variables
  P1 <- 0  # Player 1's total score
  P2 <- 0  # Player 2's total score
  T <- 0   # Turn total for the current player's turn
  player_turn <- 1  # Start with Player 1
  turn_number <- 1  # Keep track of the turn number
  
  while (P1 < TARGET_SCORE && P2 < TARGET_SCORE) {
    # VERBOSE logging for debugging
    if (VERBOSE) {
      cat("\n========== Turn", turn_number, "==========\n")
      cat("Player", player_turn, "'s Turn\n")
      cat("Player 1 Score:", P1, "| Player 2 Score:", P2, "\n")
      cat("Current Turn Total:", T, "\n")
    }
    
    # Assign player-specific variables
    if (player_turn == 1) {
      P <- P1
      O <- P2
    } else {
      P <- P2
      O <- P1
    }
    
    # Reset turn total for the new turn
    T <- 0  
    
    repeat {
      # Determine the action based on the current state
      action <- policy[T + 1, P + 1, O + 1]
      
      if (VERBOSE) {
        cat("Action chosen:", action, "\n")
      }
      
      if (action == "Hold" || action == "Terminal") {
        # Hold: Add the turn total to the player's score
        P <- P + T
        if (player_turn == 1) {
          P1 <- P
        } else {
          P2 <- P
        }
        if (VERBOSE) {
          cat("Held. Total Score after Holding:", P, "\n")
        }
        break  # End the current turn
      } else if (action == "Roll") {
        # Roll: Simulate rolling the die
        roll <- sample(1:DIE_SIDES, 1)
        if (VERBOSE) {
          cat("Rolled a", roll, "\n")
        }
        
        if (roll == 1) {
          # If a 1 is rolled, the turn total is lost
          T <- 0
          if (VERBOSE) {
            cat("Rolled a 1! Turn total lost. Ending turn.\n")
          }
          break  # End the current turn
        } else {
          # Add the roll to the turn total
          T <- T + roll
          T <- min(T, T_MAX)  # Ensure T does not exceed T_MAX
          if (VERBOSE) {
            cat("Turn Total after rolling:", T, "\n")
          }
        }
      }
      
      # Check if the player wins during the turn
      if (P + T >= TARGET_SCORE) {
        P <- P + T
        if (player_turn == 1) {
          P1 <- P
        } else {
          P2 <- P
        }
        if (VERBOSE) {
          cat("Player", player_turn, "reaches the target score and wins!\n")
        }
        break
      }
    }
    
    # Switch to the next player
    player_turn <- ifelse(player_turn == 1, 2, 1)
    turn_number <- turn_number + 1
  }
  
  # Determine the winner
  winner <- ifelse(P1 >= TARGET_SCORE, 1, 2)
  
  # VERBOSE final output
  if (VERBOSE) {
    cat("\n========== Game Over ==========\n")
    cat("Player", winner, "wins!\n")
    cat("Final Scores - Player 1:", P1, "| Player 2:", P2)
  }

  return(winner)
}

##########################################################
# `simulate_optimal_vs_heuristic_pig` function
#
# Purpose:
#   Simulates a two-player game of Pig where Player 1 uses an 
#   OPTIMAL STRATEGY (policy matrix) and Player 2 uses a HEURISTIC 
#   strategy (holding when the turn total is 20 or more).
#
# Inputs:
#   V: A 3D array representing the value function for all 
#      game states (TurnTotal, PlayerScore, OpponentScore).
#   policy: A 3D array representing the optimal action 
#           (Roll or Hold) for all game states.
#   starting_player: Specifies the player who takes the first turn 
#                    (1 for Player 1, 2 for Player 2; default is 1).
#   VERBOSE: Logical flag to print detailed game progress, 
#            including each player's turn actions and outcomes 
#            (default is FALSE).
#
# Outputs:
#   The winner of the game (1 for Player 1, 2 for Player 2).
#
# Details:
#   - Player 1 (Optimal Strategy): Actions are determined using 
#     the provided `policy` matrix derived from the value function.
#   - Player 2 (Heuristic Strategy): Holds when the turn total 
#     reaches or exceeds 20 or when it can achieve the target score.
#   - The game ends when one player's score reaches or exceeds 
#     the target score (default is 100).
#
# Notes:
#   - The policy and value function must be precomputed and provided 
#     as inputs for Player 1.
#   - This function assumes the game mechanics of Pig, including a 
#     single six-sided die, are fixed.
##########################################################


simulate_optimal_vs_heuristic_pig <- function(
    V, 
    policy, 
    starting_player=1,  # Player 1 starts by default
    T_MAX = 100,        # Maximum turn score (to limit state space)
    TARGET_SCORE = 100, # Score required to win
    DIE_SIDES = 6,      # Number of sides on the die
    VERBOSE=FALSE)
{
  # Initialize scores
  P1 <- 0  # Player 1's total score
  P2 <- 0  # Player 2's total score
  T <- 0   # Current turn total
  player_turn <- starting_player # 1 or 2
  
  turn_number <- 1
  
  while (P1 < TARGET_SCORE && P2 < TARGET_SCORE) {
    if (VERBOSE) {
      cat("\n========== Turn", turn_number, "==========\n")
      cat("Player", player_turn, "'s Turn\n")
      cat("Player 1 Score:", P1, "| Player 2 Score:", P2, "\n")
      cat("Current Turn Total:", T, "\n")
    }
    
    if (player_turn == 1) {
      # Player 1's turn (Optimal Policy)
      P <- P1
      O <- P2
      T <- 0  # Reset turn total
      
      repeat {
        action <- policy[T + 1, P + 1, O + 1]
        
        if (VERBOSE) {
          cat("Player 1 chooses action:", action, "\n")
        }
        
        if (action == "Hold" || action == "Terminal") {
          # Hold action
          P <- P + T
          P1 <- P  # Update Player 1's score
          if (VERBOSE) {
            cat("Player 1 holds. Total Score after Holding:", P1, "\n")
          }
          break  # End of turn
        } else if (action == "Roll") {
          # Roll action
          roll <- sample(1:DIE_SIDES, 1)
          if (VERBOSE) {
            cat("Player 1 rolled a", roll, "\n")
          }
          if (roll == 1) {
            # Rolled a 1, turn ends
            T <- 0
            if (VERBOSE) {
              cat("Player 1 rolled a 1! Turn total lost. Ending turn.\n")
            }
            break
          } else {
            T <- T + roll
            T <- min(T, T_MAX)  # Cap T at T_MAX
            if (VERBOSE) {
              cat("Player 1's Turn Total after rolling:", T, "\n")
            }
          }
        }
        
        # Check if Player 1 can win this turn
        if (P + T >= TARGET_SCORE) {
          P <- P + T
          P1 <- P
          if (VERBOSE) {
            cat("Player 1 reaches the target score and wins the game!\n")
          }
          break
        }
      }
    } else {
      # Player 2's turn (Heuristic Strategy)
      P <- P2
      O <- P1
      T <- 0  # Reset turn total
      
      repeat {
        if (T >= 20 || P + T >= TARGET_SCORE) {
          # Hold action
          P <- P + T
          P2 <- P  # Update Player 2's score
          if (VERBOSE) {
            cat("Player 2 holds. Total Score after Holding:", P2, "\n")
          }
          break  # End of turn
        } else {
          # Roll action
          roll <- sample(1:DIE_SIDES, 1)
          if (VERBOSE) {
            cat("Player 2 rolled a", roll, "\n")
          }
          if (roll == 1) {
            # Rolled a 1, turn ends
            T <- 0
            if (VERBOSE) {
              cat("Player 2 rolled a 1! Turn total lost. Ending turn.\n")
            }
            break
          } else {
            T <- T + roll
            if (VERBOSE) {
              cat("Player 2's Turn Total after rolling:", T, "\n")
            }
          }
        }
        
        # Check if Player 2 can win this turn
        if (P + T >= TARGET_SCORE) {
          P <- P + T
          P2 <- P
          if (VERBOSE) {
            cat("Player 2 reaches the target score and wins the game!\n")
          }
          break
        }
      }
    }
    
    # Check if the game has ended
    if (P1 >= TARGET_SCORE || P2 >= TARGET_SCORE) {
      break
    }
    
    # Switch player
    player_turn <- ifelse(player_turn == 1, 2, 1)
    turn_number <- turn_number + 1
  }
  
  # Determine winner
  winner <- ifelse(P1 >= TARGET_SCORE, 1, 2)
  
  if (VERBOSE) {
    cat("\n========== Game Over ==========\n")
    cat("Player", winner, "wins!\n")
    cat("Final Scores - Player 1:", P1, "| Player 2:", P2)
  }
  
  return(winner)
}

##########################################################
# `simulate_heuristic_vs_heuristic_pig` function
#
# Purpose:
#   Simulates a two-player game of Pig where both players 
#   use the same HEURISTIC STRATEGY (holding when the turn 
#   total meets or exceeds a specified threshold).
#
# Inputs:
#   heuristic: The turn total at which both players hold 
#              (default is 20).
#   starting_player: Specifies the player who takes the first turn 
#                    (1 for Player 1, 2 for Player 2; default is 1).
#   VERBOSE: Logical flag to print detailed game progress, 
#            including each player's turn actions and outcomes 
#            (default is FALSE).
#
# Outputs:
#   The winner of the game (1 for Player 1, 2 for Player 2).
#
# Details:
#   - Both players follow the same heuristic strategy: 
#     Hold when the turn total meets or exceeds the `heuristic` value 
#     or when they can achieve the target score during the turn.
#   - Rolling a 1 resets the current turn total and ends the turn.
#   - The game ends when one player's score reaches or exceeds the 
#     target score (default is 100).
#
# Notes:
#   - The heuristic threshold can be adjusted to simulate more or less 
#     aggressive strategies.
#   - This function assumes the game mechanics of Pig, including a 
#     single six-sided die, are fixed.
##########################################################

simulate_heuristic_vs_heuristic_pig <- function(
    heuristic=20, 
    starting_player=1, 
    TARGET_SCORE=100, # score required to win
    DIE_SIDES=6,      # number of sides on the die
    VERBOSE=FALSE
) {
  P1 <- 0  # Player 1's total score
  P2 <- 0  # Player 2's total score
  T <- 0   # Current turn total
  player_turn <- starting_player  # 1 or 2
  
  turn_number <- 1
  
  while (P1 < TARGET_SCORE && P2 < TARGET_SCORE) {
    if (VERBOSE) {
      cat("\n========== Turn", turn_number, "==========\n")
      cat("Player", player_turn, "'s Turn\n")
      cat("Player 1 Score:", P1, "| Player 2 Score:", P2, "\n")
      cat("Current Turn Total:", T, "\n")
    }
    
    # Assign player-specific variables
    if (player_turn == 1) {
      P <- P1
      O <- P2
    } else {
      P <- P2
      O <- P1
    }
    
    T <- 0  # Reset turn total
    
    # Heuristic Strategy: Hold when T >= 20 (default) or can win this turn
    repeat {
      if (T >= heuristic || P + T >= TARGET_SCORE) {
        # Hold action
        P <- P + T
        if (player_turn == 1) {
          P1 <- P  # Update Player 1's score
        } else {
          P2 <- P  # Update Player 2's score
        }
        if (VERBOSE) {
          cat("Player", player_turn, "holds. Total Score:", P, "\n")
        }
        break  # End of turn
      } else {
        # Roll action
        roll <- sample(1:DIE_SIDES, 1)
        if (VERBOSE) {
          cat("Player", player_turn, "rolled a", roll, "\n")
        }
        if (roll == 1) {
          # Rolled a 1, turn ends
          T <- 0
          if (VERBOSE) {
            cat("Player", 
                player_turn, 
                "rolled a 1! Turn total lost. Ending turn.\n")
          }
          break
        } else {
          T <- T + roll
          if (VERBOSE) {
            cat("Player", 
                player_turn, 
                "'s Turn Total after rolling:", 
                T, 
                "\n")
          }
        }
      }
    }
    
    # Check if the game has ended
    if (P1 >= TARGET_SCORE || P2 >= TARGET_SCORE) {
      break
    }
    
    # Switch player
    player_turn <- ifelse(player_turn == 1, 2, 1)
    turn_number <- turn_number + 1
  }
  
  # Determine winner
  winner <- ifelse(P1 >= TARGET_SCORE, 1, 2)
  
  if (VERBOSE) {
    cat("\n========== Game Over ==========\n")
    cat("Player", winner, "wins!\n")
    cat("Final Scores - Player 1:", P1, "| Player 2:", P2)
  }
  
  return(winner)
}
