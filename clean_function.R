library(DataComputing)
uncleaned <- read.csv('~/Downloads/SpeedDating.csv')
analyze_variables <- function(trait_vec, match = TRUE) {
  if (match) { 
    df <- filter(uncleaned, match == 1)
  } else {
    df <- filter(uncleaned, match == 0)
  }
  names <- c()
  for (trait in trait_vec) {
    names <- c(names, paste(trait, "0"))
    names <- c(names, paste(trait, "1"))
  }
  new_index = 1
  toReturn <- data.frame(matrix(nrow = 0, ncol = length(names)))
  names(toReturn) <- names
  for (i in 1:nrow(df)) { 
    if (df[i, 'iid'] < df[i, 'pid']) {
      for (trait in trait_vec) {
        if (is.factor(df[i, trait])) {
          toReturn[new_index, paste(trait, '0')] = as.character(df[i, trait])
          toReturn[new_index, paste(trait, '1')] = as.character(filter(df, iid == df[i, 'pid'], pid == df[i, 'iid'])[1,trait])
        } else {
          toReturn[new_index, paste(trait, '0')] = df[i, trait]
          toReturn[new_index, paste(trait, '1')] = filter(df, iid == df[i, 'pid'], pid == df[i, 'iid'])[1,trait]
        }
      }
      new_index <- new_index + 1
    }
  }
  return (toReturn)
}

analyze_variables(c('field'))
