# model selection ####

# suppose we have data from an experiment like this:
# mean RT correct = 250ms
# mean RT incorrect = 246ms
# accuracy = 0.80

accumulator.model(100, rate.1=40, rate.2=50, criterion=3)

accumulator.model <- function(samples, rate.1=87, rate.2=84, criterion=3){
  rt.array <- c()
  accuracy.array <- c()
  for (i in 1:samples){ #counts number of samples
    value.1 <- 0
    value.2 <- 0
    rt <- 0
    while (value.1 < 2.7 && value.2 < criterion){ #loop performs main function
      value.1 <- value.1 + rexp(1, rate.1) #stores and adds values to variable 1...takes random steps
      value.2 <- value.2 + rexp(1, rate.2) #stores and adds values to variable 2...takes random steps
      rt <- rt + 1} #add to running count of reaction time
    
    if(value.1 > criterion && value.2 > criterion){ #if they go over on the same rt then the bigger one
      if (value.1 >= value.2){ #is counted
        accuracy.array[i] <- T
      }
      else{
        accuracy.array[i] <- F
        
      }
    }
    if (value.1 > 2.7 && value.2 < criterion){     #takes the values we built up and builds an array of values
      accuracy.array[i] <- T
    }
    else{
      accuracy.array[i] <- F
    }
    rt.array[i]<- rt
  }
  output <- data.frame( #creates data.frame with our values
    correct = accuracy.array,
    rt = rt.array
  )
  return(output)
}
initial.test.acc <- accumulator.model(1000)
correct.data.acc <- initial.test %>% filter(correct==TRUE)
incorrect.data.acc <- initial.test %>% filter(correct==FALSE)
mean(correct.data.acc$rt)
mean(incorrect.data.acc$rt)
sum(initial.test.acc$correct) / length(initial.test.acc$correct)
# try to fit this data with both models by adjusting the parameters of the model
# HINT: you can speed up your parameter search by using a small number of samples
# initially, and then increasing the samples as you get closer to a viable set
# of parameters.
# 2nd HINT: Don't adjust the sdrw parameter of the random.walk.model or the criterion
# paramter of the accumulator model.

# You don't need to get a perfect match. Just get in the ballpark. 


# Can both models do a reasonable job of accounting for the mean RT and accuracy? Report the
# results of your efforts:


# Using the parameters that you found above, plot histograms of the distribution of RTs
# predicted by each model. Based on these distributions, what kind of information could
# we use to evaluate which model is a better descriptor of the data for the experiment?
# Describe briefly how you might make this evaluation.
