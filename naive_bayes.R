niave_bayes <- function(datafr,lambda=0){
  num_features <- ncol(datafr) - 1
  num_obs <- nrow(datafr)
  num_class <- length(unique(datafr[,num_features+1]))
# Calculating conditional probabilities  
  for (i in 1:num_features) {
    tab <- table(datafr[,i],datafr[,num_features+1]) + lambda
    sumtab <- apply(tab,2,sum)
    if (i==1){
      conprob <- sweep(tab,2,sumtab,'/')      
    }
    else
    {
      update <- sweep(tab,2,sumtab,'/')
      conprob <- rbind(conprob,update)
    }
  }
# Calculating predictions
  for (i in 1:num_obs) {
    obs <- conprob[as.character(datafr[i,1:num_features]),]
    obsprob <- apply(obs,2,prod)
    if (i==1) {
      preds <- names(which.max(obsprob))
    }
    else{
      preds <- c(preds,names(which.max(obsprob)))
    }
  }
  error <- 100*(1-sum(preds==datafr[,num_features+1])/length(datafr[,1]))
  model <- list(conprob,datafr[,num_features+1],preds,error)
  return(model)
}
  