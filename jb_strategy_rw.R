library(MASS)

set.seed(1)


# Question 1 Strategy C --------------------------------------------------------
roulette <- function(n, numbers=0:36){
  # n = number of rolls
  rolls <- sample(numbers, size=n, replace=TRUE)
  return(rolls)
}

# green is a straight up bet => x35 payout
# 13 to 18 is a Line bet => x5 payout
# 19 to 36 is High bet => x1 payout
jb_profit <- function(roll, bet=10){
  # for a single roll, calculate winnings
  if (roll >= 19 & roll <= 36) return(0.7*bet - 0.3*bet)
  if (roll >=13 & roll <= 18) return(5*0.25*bet - 0.75*bet)
  if (roll == 0) return(35*0.05*bet - 0.95*bet)
  
  return(-bet)
}

simulate_games <- function(n=1000, rpg=500){
  # Function returns the profits from n games with rpg rolls per game
  # P is the matrix where the entry in position P_{i, j} is the payout 
  # for roll i of game j
  pnl <- vector(length=n)
  P <- matrix(ncol=n, nrow=rpg)  # payout matrix, stores the payout of each roll in the game
  
  
  for (i in 1:n){
    game <- roulette(rpg)
    
    payout_per_roll <- sapply(game, jb_profit)
    P[,i] = payout_per_roll
    pnl[i] <- sum(payout_per_roll)
  }
  
  return(list(pnl, P))
}

results <- simulate_games()
pnl <- results[[1]]
payout_per_roll <- results[[2]]


# the expected gain/loss falls around -135
# variance is around 22800
truehist(pnl, main='Distribution of total Profit/Loss after 500 roulette rolls using\n
the James Bond Strategy', xlab='pnl', ylab='density', cex.main=0.7)

ev <- mean(pnl); ev
variance <- var(pnl); variance
stdev <- sd(pnl); stdev

# want more certainty of estimates...
n_sims <- 100
evs <- vector(length=n_sims)
vars <- vector(length=n_sims)

for (i in 1:n_sims){
  results <- simulate_games()
  evs[i] <- mean(results[[1]])
  vars[i] <- var(results[[1]])
}

truehist(evs, nbins=5); mean(evs)
truehist(vars); mean(vars)
mean(sqrt(vars))

# Question 2 Strategy C --------------------------------------------------------

# what is the expected gain/loss on a single roll, and does it converge?
n = 10000
profit_vec <- sapply(roulette(n), jb_profit)
x_hat <- vector(length=n)
for (i in 1:n){
  x_hat[i] <- mean(profit_vec[1:i])
}

plot(1:n, x_hat[1:n], type='l', 
     xlab='rolls', 
     ylab= 'profit',
     main='Sample mean of profit/loss for a single roulette roll, \naveraged over n rolls',
     cex.main=0.7
)

# the expected loss for a single roll is slightly negative
x_hat[n]

# sensitivity test 1
# if we incrementally increase the number of rolls per game, how does this effect the 
# expected profit and loss?
  
rolls_vec <- seq(500, 1000, 50)

n <- length(rolls_vec)
t1_ev_winnings <- vector(length=n)

for (i in 1:n){
   result <- simulate_games(rpg=rolls_vec[i])
   pnl <- result[[1]]
   truehist(pnl, main=paste0('Distribution of total Profit/Loss after ',
                             rolls_vec[i],
                             ' roulette rolls using\n the James Bond Strategy'), 
            xlab='pnl', ylab='density', cex.main=0.7)
   t1_ev_winnings[i] <- mean(pnl)
}

plot(1:n, t1_ev_winnings, type='l', main='Expected profit vs Number of rolls',
     xlab = "Number of rolls", ylab='Expected profit')


# JB Strategy with stop conditions ----------------------------------------
simulate_games_stop <- function(n=1000, rpg=500, stoploss=-50, stopgain=10){
  # Function returns the profits from n games with rpg rolls per game
  # P is the matrix where the entry in position P_{i, j} is the payout 
  # for roll i of game j
  pnl <- vector(length=n)
  P <- matrix(ncol=n, nrow=rpg)  # payout matrix, stores the payout of each roll in the game
  
  
  for (i in 1:n){
    game <- roulette(rpg)
    
    payout_per_roll <- sapply(game, jb_profit)
    
    c_payout <- cumsum(payout_per_roll)
    # get the index of the first violation of stop conditions
    for (j in 1:rpg)
      {
      if (c_payout[j] <= stoploss | c_payout[j] >= stopgain) 
        {
        payout_per_roll <- payout_per_roll[1:j]
        break
      }
    }
    
    print(paste0("stopped after ", j, " rolls"))
    
    print(payout_per_roll)
    pnl[i] <- sum(payout_per_roll)
    P[1:j,i] = payout_per_roll
    
  }
  
  return(list(pnl, P))
}

results <- simulate_games_stop()
pnl <- results[[1]]
payout_per_roll <- results[[2]]


# the expected gain/loss falls around -135
# variance is around 22800
truehist(pnl, main='Distribution of total Profit/Loss after 500 roulette rolls using\n
the James Bond Strategy', xlab='pnl', ylab='density', cex.main=0.7)

ev <- mean(pnl); ev
variance <- var(pnl); variance
stdev <- sd(pnl); stdev
