# Hello world
a <- sample(10)
a
plot(a, type = 'b')

# Factor
food <- factor(c("low", "high", "medium", "high", "low", "medium", "high"))
levels(food)


# Into the cereal box
library(parallel)

getCard <- function(noCards, whichCard){
  noDraws <- 0
  while (sample(1:noCards, 1) != whichCard)
    noDraws <- noDraws + 1
  return (noDraws)
}


makeExperiment <- function(experimentDraws, noCards){
  experimentDraws <- 0
  start <- Sys.time()
  
  for (j in 1:noCards)
    experimentDraws <- experimentDraws + getCard (noCards, j)
  
  diff <- Sys.time() - start
  return (c(experimentDraws, diff))
}


getExperimentsResults <- function(noExperiments, noCards){
  # experiments is just a placeholder, I didn't find a substitute for it
  experiments <- rep (0, noExperiments)
  # run this function on all cores
  res <- mclapply(experiments, makeExperiment, noCards = noCards, mc.cores = detectCores())
  return (res)
}

start <- Sys.time()
res <- getExperimentsResults(100, 11)
res <- unlist(res)
sprintf('Experiments took %f seconds to execute', Sys.time() - start)
# get draws and running times
# draws are on uneven positions (1, 3, ...) and running times on even position
draws <- res[seq(1, length(res), by = 2)]
runningTimes <- res[seq(2, length(res), by = 2)]
sprintf ('Average draws: %f', sum(draws) / length(draws))
sprintf ('Average running time for an experiment: %f', sum(runningTimes) / length(runningTimes))

# Plot experiments with the average value
plot(draws, type='b')
abline(h = sum(draws) / length(draws))

# Export to excel
library(openxlsx)
res <- data.frame(draws, runningTimes)
print(res)
write.xlsx(res, "./draws.xlsx")
