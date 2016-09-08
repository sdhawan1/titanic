#FOR THE CONSOLE: setwd("/home/sidharth/Documents/kaggle")


#This algorithm looks like a job for object oriented programming. Here is a link that describes OO programming in R: 
#   http://www.cyclismo.org/tutorial/R/s3Classes.html

#create two classes: one class for the (decision stump) hypotheses, another file for the Central adaboost file.


#------------------------------------------------------------------------------------------------------------#

#define the Adaboost class (should be an S3 class).
AdaBoost <- function(trainData, k0) {
  
  #get the environment for this instance of function:
  thisEnv <- environment()
    
  k <- k0  
  #need to define 'hypotheses' as a list of k decision stumps.
  # EITHER MAKE THE ERROR AN ATTRIBUTE OF THE HYPOTHESIS, OR HAVE A SEPARATE HYPWEIGHT LIST [leaning towards former.]
  hyps <- list()  
  #need list of data weights (to train new hyp.)
  # MAY WANT TO MAKE THIS PART OF THE TRAINING DATA?
  # NOT SURE HOW THIS IS SUPPOSED TO BE INITIALIZED
  trainDataWts <- sapply(1:nrow(trainData), function(x) 1/nrow(trainData))
  
  
  me <- list (
    #define the environment where the list is defined.
    thisEnv = thisEnv,
    
    #function to return the environment; example of environments and the 'get' function.
    getEnv = function() {
      #the first argument of get is the environment to search within.
      #The second argument is the name of the variable we want from that env.
      return(get("thisEnv", thisEnv))
    }
    
    #INCLUDE A FUNCTION TO RETURN THE PREDICTION OF THE ADABOOST MODEL ON A TEST DATASET [WRITE SOMEWHERE]?
    
    #perhaps put other functions in here. Maybe define more complex functions outside.    
    )  
  
  #define value of the list within the current environment.
  assign('this', me, envir=thisEnv)
  
  #set name of class
  class(me) <- append(class(me), "AdaBoost")
  
  
  
  ##### Initialization: Given a set of training data, find the best Adaboost model to fit it.
  #step one: create a set of all possible hypotheses for dataset; categorize all vbls. Call this function from "decstump" file
  stumpEnvInstance = stumpEnv(trainData)
  #repeat the below for k decision stumps, where k is a user-defined input for the number of iterations.
  for (ki in 1:k) {
    #Call a function that creates a new decision stump [this function should return the optimal hypothesis, its error, 
    #   and a (boolean) list of which values were right and wrong.]
    #AR: DEFINE THE FUNCTION: "GETDECISIONSTUMP": should take parameter for "stump environment", which stores var types & hyps.
    hyp_err = getDecisionStump(trainData, trainDataWts, stumpEnvInstance)
    #add the returned hypothesis to hypothesis list. Weight of hyp should be pre-calculated and included here.
    hyps = c(hyps, hyp_err[1])
    err = hyp_err[2]
    whitelist = hyp_err[3]
    
    #recalculate weights of training data values: w[corr] = w[corr]; w[wrong] = w[wrong] * [err/(1-err)]
    wscale = err/(1-err)
    trainDataWts = sapply(l:length(trainDataWts), function(x) if(whitelist[x]) trainDataWts[x] else trainDataWts[x] * wscale)
    #now, normalize trainDataWts [?]
    trainDataWts = trainDataWts / sum(trainDataWts)
  }
  
  return(me)
}


### out here, write a function to return the prediction of an adaboost model for a given dataset.



#------------------------------------------------------------------------------------------------------------#
### INCLUDE THE BELOW IN ITS OWN FILE??

#Needed for the tester function: modification of the data needed to train the Adaboost model.
processTitanicTestCSV <- function(trainInput) {
  #clearly mark the column that identifies each row.
  trainData <- trainInput
  names(trainData)[1] <- "ID"
  names(trainData)[2] <- "Result"
  #remove some variables which I don't think carry any relevant information?? Or maybe just let the algorithm figure it out??
  
  #in the end, return "trainData"
  return(trainData)
}

##Later: maybe write a similar function to modify the test output.

##### Tester Function: Given a set of training data & testing data, generate a model and run it on all of the test data.
#for every new input, return a weighted majority of stumps.
#Params: trainData & testData: training and testing datasets. k: number of Adaboost iterations to run.
runAdaBoost <- function(trainInput <- read.csv("train.csv"), testInput <- read.csv("test.csv"), k) {
  ###TODO: MODIFY / PROCESS THE TRAINDATA FILE AS INPUTTED TO THE ADABOOST FUNCTION. TAKE OUT USELESS VARIABLES, CLEARLY MARK ID & RESULT.
  trainData <- processTitanicTestCSV(trainInput)
  
  #Todo: define the class "adaboost"; esp. need the initialization function. 
  # This should return an instance of class "AdaBoost"
  adaBoostInstance <- AdaBoost(trainData, k)
  
  #Todo: define a function that runs Adaboost on a set of test data.
  #This should run Adaboost on the test data and return the percentage of outcomes that are correct.
  predictions <- adaBoostInstance$getPredictions(testData)
  
  #Now, test what percentage of these predictions are correct and return a score [can't do this - just print out / return the results.]
  #print(predictions)
  return(predictions)
}


