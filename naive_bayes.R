naive_bayes <- function(datafr,lambda=0){
  num_features <- ncol(datafr) - 1
  num_obs <- nrow(datafr)
  num_class <- length(unique(datafr[,num_features+1]))
# Calculating conditional probabilities  
  for (i in 1:num_features) {
    tab <- table(datafr[,i],datafr[,num_features+1]) + lambda
    if (i==1){
      all <- apply(tab,2,sum)
      conprob <- sweep(tab,2,all,'/')      
    }
    else
    {
      update <- sweep(tab,2,all,'/')
      conprob <- rbind(conprob,update)
    }
  }
  all <- all/sum(all)
  conprob <- rbind(conprob,all)
# Calculating predictions
  for (i in 1:num_obs) {
    obs <- conprob[c(as.character(datafr[i,1:num_features]),"all"),]
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
  