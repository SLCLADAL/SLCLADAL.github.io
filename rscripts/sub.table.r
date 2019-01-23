sub.table <- function(wholetable, rel.rows, rel.cols, out="short") {

   # sub-table
   subtable <- wholetable[rel.rows, rel.cols]

   # row percentages observed in whole table
   row.perc.whole <- matrix(rep(rowSums(wholetable[rel.rows,])/sum(rowSums(wholetable[rel.rows,])), length(rel.cols)), ncol=length(rel.cols))

   # column percentages observed in whole table
   col.perc.whole <- matrix(rep(colSums(wholetable[,rel.cols])/sum(colSums(wholetable[,rel.cols])), each=length(rel.rows)), ncol=length(rel.cols))

   # row totals observed in sub-table
   row.sum.sub <- matrix(rep(rowSums(subtable), length(rel.cols)), ncol=length(rel.cols))

   # column totals observed in sub-table
   col.sum.sub <- matrix(rep(colSums(subtable), each=length(rel.rows)), ncol=length(rel.cols))

   t1 <- row.sum.sub*col.perc.whole; attr(t1, "dimnames") <- attr(subtable, "dimnames") # expected acc. to row totals of whole table; p. 147, top left
   t2 <- row.perc.whole*col.sum.sub; attr(t2, "dimnames") <- attr(subtable, "dimnames")  # expected acc. to column totals of whole table; p. 147, top right
   t3 <- sum(subtable)*row.perc.whole*col.perc.whole; attr(t3, "dimnames") <- attr(subtable, "dimnames")  # expected acc. to totals of whole table; p. 147, below top

   test.for.globalfit <- c("Chi-square"=sum(((subtable-t3)^2)/t3), "Df"=prod(dim(subtable))-1, "p-value"=pchisq(sum(((subtable-t3)^2)/t3), prod(dim(subtable))-1, lower.tail=FALSE)) # test of global fit; p. 144 bottom (acc. to Eq. 5.71)

   chisquare.contingency <- c("Chi-square"=sum((subtable-t1-t2+t3)^2/t3), "Df"=prod(dim(subtable)-1), "p-value"=pchisq(sum((subtable-t1-t2+t3)^2/t3), prod(dim(subtable)-1), lower.tail=FALSE)) # test for contingency; p. 145 (acc. to Eq. 5.74)


   test.for.rows <- chisq.test(rowSums(subtable), p=rowSums(wholetable[rel.rows,])/sum(rowSums(wholetable[rel.rows,]))) # test for fit of rows; p. 148 top (acc. to Eq. 5.72)
   chisquare.rows <- c("Chi-square"=as.numeric(test.for.rows[[1]]), "Df"=as.numeric(test.for.rows[[2]]), "p-value"=as.numeric(test.for.rows[[3]]))
   row.result <- round(test.for.rows$exp, 2) # = rowSums(t3)
   names(row.result) <- rownames(subtable)

   test.for.cols <- chisq.test(colSums(subtable), p=colSums(wholetable[,rel.cols])/sum(colSums(wholetable[,rel.cols]))) # test for fit of columns; p. 148 below that (acc. to Eq. 5.73)
   chisquare.columns <- c("Chi-square"=as.numeric(test.for.cols[[1]]), "Df"=as.numeric(test.for.cols[[2]]), "p-value"=as.numeric(test.for.cols[[3]]))
   col.result <- round(test.for.cols$exp, 2) # = colSums(t3)
   names(col.result) <- colnames(subtable)

   # output
   chisquares <- c(test.for.globalfit[1], chisquare.rows[1], chisquare.columns[1], chisquare.contingency[1])
   dfs <- c(test.for.globalfit[2], chisquare.rows[2], chisquare.columns[2], chisquare.contingency[2])
   ps <- c(test.for.globalfit[3], chisquare.rows[3], chisquare.columns[3], chisquare.contingency[3])

   output <- data.frame(chisquares, dfs, ps)
   rownames(output) <- c("Cells of sub-table to whole table", "Rows (within sub-table)", "Columns (within sub-table)", "Contingency (within sub-table)")
   colnames(output) <- c("Chi-square", "Df", "p-value")

   if (out=="short") {
      return(list("Whole table"=addmargins(wholetable), "Sub-table"=addmargins(subtable), "Chi-square tests"=output))
      # return(list("Whole table"=addmargins(wholetable), "Sub-table"=addmargins(subtable), "chi-square for global fit of sub-table to whole table"=test.for.globalfit,
      # "Chi-square for contingency of sub-table"=chisquare.contingency, "Chi-square for rows of sub-table"=chisquare.rows, "Chi-square for columns of sub-table"=chisquare.columns))
   } else {
      return(list("Whole table"=addmargins(wholetable), "Sub-table"=addmargins(subtable), "Chi-square tests"=output, "Cells expected in sub-table from whole table"=round(t3, 2), "Row totals expected in sub-table from whole table"=row.result, "Column totals expected in sub-table from whole table"=col.result))
   }
}
