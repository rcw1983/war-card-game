library(tidyverse)

deck = (c(0:51) %% 13) + 1

r <- sample(1:52, 52, replace = FALSE)

s <- list(
  h1 = deck[r[1:26]],
  h2 = deck[r[27:52]],
  stack = c()
)

one_hand <- function(df) {
  
  if(length(df$h1) == 0) {
    df$h2 <- c(df$h2, df$stack)
    df$stack <- c()
    return(df)
  } else if(length(df$h2) == 0) {
    df$h1 <- c(df$h1, df$stack)
    df$stack <- c()
    return(df)
  }
  
  df$stack <- c(df$stack, df$h1[1], df$h2[1])
  df$h1 <- df$h1[-1]
  df$h2 <- df$h2[-1]
  
  if(df$stack[length(df$stack) - 1] > df$stack[length(df$stack)]) {
    
    df$stack <- sample(df$stack, length(df$stack), replace = FALSE)
    df$h1 <- c(df$h1, df$stack)
    df$stack <- c()
    
  } else if(df$stack[length(df$stack)] > df$stack[length(df$stack) - 1]) {
    
    df$stack <- sample(df$stack, length(df$stack), replace = FALSE)
    df$h2 <- c(df$h2, df$stack)
    df$stack <- c()
    
  } else {
    
    if(length(df$h1) < 4) {
      df$h2 <- c(df$h2, df$stack, df$h1)
      df$h1 = c()
      df$stack <- c()
      return(df)
    } else if(length(df$h2) < 4) {
      df$h1 <- c(df$h1, df$stack, df$h2)
      df$h2 = c()
      df$stack <- c()
      return(df)
    }
    
    df$stack <- c(df$stack, df$h1[1:3], df$h2[1:3])
    df$h1 <- df$h1[-(1:3)]
    df$h2 <- df$h2[-(1:3)]
    
    df <- one_hand(df)
    
  }
  
  return(df)
  
}

play_war <- function(df, n_turns = 1, n_cards = c()) {
  
  n_cards[n_turns] <- length(df$h1)
  
  if(length(df$h1) == 0 || length(df$h2) == 0) {
    return(n_cards)
  } else {
    return(play_war(one_hand(df), n_turns + 1, n_cards))
  }
  
}

play_war(s)
