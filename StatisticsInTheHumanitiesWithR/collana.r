# CollocationAnalysis
collex <- function(data = data, cv1 = cv1){
  # set up rslttb
  rslttb <- matrix(c("int", "adj", "or", "p"), ncol = 4)
  colnames(rslttb) <- c("Intensifier", "Adjective", "OddsRatio", "p-Value")
  rvs <- 1:nrow(t3)
  # define column values
  cv0 <- 1
  # set up table
  sapply(rvs, function(x){
    # extract values
    b2 <- t3[x,cv1] # freq adj with int
    b3 <- sum(t3[x,])-b2 # freq adj without int
    c2 <- sum(t3[,cv1])-b2   # freq int general
    c3 <- sum(t3[,cv0])-t3[x,cv0] # freq adj without int general
    # set up table
    collextb <- matrix(c(b2, b3, c2, c3), ncol = 2, byrow = F)
    # add row names
    rownames(collextb) <- c("Int", "NoInt")
    # add column names
    colnames(collextb) <- c("Adj", "AdjGen")
    # perform fisher's exact test
    rslt <- fisher.test(collextb)
    # set up table with results
    rslttb <- list(c(colnames(data)[cv1], rownames(data)[x],
      as.vector(unlist(rslt[3])), as.vector(unlist(rslt[1]))))
    # return results
    return(rslttb)
    } )
    }
