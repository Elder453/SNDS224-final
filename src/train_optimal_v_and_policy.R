##########################################################
# `train_optimal_v_and_policy` function
#
# Purpose:
#   Computes the value function and policy for a 
#   two-player game of Pig using **value iteration**.
#   The function trains an optimal strategy by evaluating
#   all possible states and actions to determine the best 
#   policy.
#
# Inputs:
#   TARGET_SCORE: The score required to win the game 
#                 (default is 100).
#   DIE_SIDES: The number of sides on the die (default is 6).
#   T_MAX: Maximum turn total considered in state space 
#          (default is 100).
#   theta: Convergence threshold for value iteration 
#          (default is 1e-4).
#   VERBOSE: Logical flag to print progress at each iteration 
#            of value iteration (default is TRUE).
#
# Outputs:
#   A list containing:
#     - V: The optimal value function array of dimension 
#          (T_MAX + 1, TARGET_SCORE + 1, TARGET_SCORE + 1), 
#          where V[T + 1, P + 1, O + 1] represents the value 
#          of having a turn total of T, player score P, and 
#          opponent score O.
#     - policy: The optimal policy array of dimension 
#          (T_MAX + 1, TARGET_SCORE + 1, TARGET_SCORE + 1), 
#          where policy[T + 1, P + 1, O + 1] is either "Roll" 
#          or "Hold" for the corresponding state.
#
# Notes:
#   - The game transitions from the player's turn to the 
#     opponent's turn upon holding or rolling a 1.
#   - Opponent is assumed to play optimally, using the same 
#     value function.
#   - Value iteration updates the value function and policy 
#     until the maximum change in value across all states 
#     (delta) is smaller than `theta`.
##########################################################

train_optimal_v_and_policy <- function(
    theta=1e-5,       # convergence threshold
    T_MAX=100,        # maximum turn score (to limit state space)
    TARGET_SCORE=100, # score required to win
    DIE_SIDES=6,      # number of sides on the die
    VERBOSE=FALSE     # print progress at each iteration
) {
  delta <- Inf   # Improvement over old policy
  
  # Initialize value function V(s) and policy π(s)
  V      <- array(0, 
                  dim = c(T_MAX + 1, TARGET_SCORE + 1, TARGET_SCORE + 1))
  policy <- array("Hold", 
                  dim = c(T_MAX + 1, TARGET_SCORE + 1, TARGET_SCORE + 1))
  
  # Value Iteration Loop
  iteration <- 0
  while (delta > theta) {
    delta <- 0
    iteration <- iteration + 1
    V_old <- V
    
    for (T in 0:T_MAX) {
      for (P in 0:TARGET_SCORE) {
        for (O in 0:TARGET_SCORE) {
          
          # Terminal States
          if (P >= TARGET_SCORE) {
            V[T + 1, P + 1, O + 1]      <- 1  # Win
            policy[T + 1, P + 1, O + 1] <- "Terminal"
            next
          }
          if (O >= TARGET_SCORE) {
            V[T + 1, P + 1, O + 1]      <- -1  # Loss
            policy[T + 1, P + 1, O + 1] <- "Terminal"
            next
          }
          
          # Initialize action values
          action_values <- c(Roll = 0, Hold = 0)
          
          # Action: Hold
          if (P + T >= TARGET_SCORE) {
            action_values["Hold"] <- 1 # Immediate player win
          } else {
            # Switch to opponent's turn (assumed optimal play)
            opponent_state_value <- -V[1, O + 1, P + T + 1]
            action_values["Hold"] <- opponent_state_value
          }
          
          # Action: Roll
          expected_roll_value <- 0
          for (roll in 1:DIE_SIDES) {
            p_roll <- 1 / DIE_SIDES
            if (roll == 1) {
              # Roll 1: Lose turn total, switch to opponent
              opponent_state_value <- -V[1, O + 1, P + 1] # Zero-sum framework
              expected_roll_value  <- expected_roll_value + p_roll * opponent_state_value
            } else {
              # Roll 2-6: Add roll to turn total
              T_new               <- min(T + roll, T_MAX) # Cap turn total
              expected_roll_value <- expected_roll_value + p_roll * V[T_new + 1, P + 1, O + 1]
            }
          }
          action_values["Roll"] <- expected_roll_value
          
          # Update value function and policy
          best_action <- names(which.max(action_values))
          V[T + 1, P + 1, O + 1]      <- action_values[best_action]
          policy[T + 1, P + 1, O + 1] <- best_action
          
          # Update delta to check for convergence
          delta <- max( delta, abs(V[T + 1, P + 1, O + 1] - V_old[T + 1, P + 1, O + 1]) )
        }
      }
    }
    if (VERBOSE) {cat("Iteration:", iteration, "Delta:", delta, "\n")}
  }
  return(list(V = V, policy = policy))
}
