
#TTR calculation using the "fraction of INRs in range method"

ttr.calculation.fun <- function(av_out){
ttr.df <- c()
for (i in 1:(dim(av_out)[2]/3)) {
  inrs <- paste("av_out$INR.", i , sep="")
  checks <- paste("av_out$Check.", i , sep="")
  inrs <- eval(parse(text = inrs))
  checks <- eval(parse(text = checks))
  checks.day <- checks[checks != 0]
  inrs.for.ttr <- inrs[checks.day]
  inrs.in.range <- inrs.for.ttr[inrs.for.ttr >= 2 & inrs.for.ttr <= 3]
  calculated.ttr <- length(inrs.in.range)/length(inrs.for.ttr)*100
  ttr.df <- c(ttr.df, calculated.ttr)  
}
return(ttr.df)
}