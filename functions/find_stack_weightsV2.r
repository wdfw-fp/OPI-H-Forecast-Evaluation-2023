#optimization algorithm to find best stacking weights. modified  from Dorman et al 2018

find_stack_weightsV2<-function(metric,preds,obs){
  
  
  preds<-as.matrix(preds)
  obs<-obs
  metric<-metric
 

    # this function computes the optimal weight for a single train/test split;
    # from the models fitted to the training data it uses the predictions to the test;
    # then it optimizes the weight vector across the models for combining these 
    # predictions to the observed data in the test;
    # trick 1: each weight is between 0 and 1: w <- exp(-w)
    # trick 2: weights sum to 1: w <- w/sum(w)
    #
    # weights are weights for each model, between -infty and +infty!
    # preds are predictions from each of the models
    
#     if (NCOL(test.preds) >= length(test.obs)) stop("Increase the test set! More models 
# 	   than test points.")
    

    weightsopt <- function(ww){ 
      # function to compute metric on test data
      w <- c(1, exp(ww)); w <- w/sum(w) ## w all in (0,1) SIMON; 
      # set weight1 always to 1, other weights are scaled accordingly 
      # (this leads to a tiny dependence of optimal weights on whether model1 is any 
      # good or utter rubbish; 
      # see by moving the 1 to the end instead -> 3rd digit changes)
      pred <- as.vector(preds %*% w)
      
      
      if(metric == "MSA"){
        LAR <- log(obs/pred)
        MSA <- 100*(exp(median(abs(LAR)))-1)
        MSA
      }else{if(metric == "MAPE"){
        Error <- pred - obs
        PE <- Error/obs
        APE <- abs(PE)
        MAPE <- mean(APE)
        MAPE
      }else{if(metric == "MPE"){
        Error <- pred - obs
        PE <- Error/obs
        MPE <- mean(PE)
        MPE
      }else{if(metric == "RMSE"){
        Error <- pred - obs
        SE <- Error^2
        RMSE <- sqrt(mean(SE))
        RMSE
      }}}}
    
    }
    
    
    
    #run optimization 100 times with different starting weights
    weights<-numeric(NCOL(preds)-1) #vector to hold best weights
    skill<-Inf                      #best skill
    cnt<-0                          
    while(cnt<100){
    ops <- optim(par=rnorm(NCOL(preds)-1), weightsopt, method="BFGS")
    if (ops$convergence != 0) next 
    if(ops$value<skill){
      weights<-c(1, exp(ops$par))/sum(c(1, exp(ops$par)))
      skill<-ops$value
    }
    cnt<-cnt+1
    }
    
  results<-list(weights,skill)
  return(results)
}
