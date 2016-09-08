#IDEA: maybe create all the possible hypotheses in this environment. A hypothesis maybe should
#   store the variable that the hypothesis is about, plus the value of that variable that is
#   guessed.
#   The hypothesis testing function should be more complex; it should cross check hyp. var. val. w/
#   real variable value, and return a prediction for survival based on this.

## File: Define the "Decision Stump Environment" Object. Should be a wrapper to help create the dec. stump.
##   should contain the categorization of each variable into 'binary', 'categorical', or 'continuous'.
##   should also contain the factorized values of these variables in a separate data structure.
##   This object should be used to provide the data necessary to create a decision stump.

stumpEnv <- function(trainData) {
  #get the environment of this instance of the function:
  thisEnv <- environment()
  
  #create an object detailing the "type" of each variable.
  vartypes <- list()
  length(vartypes) <- length(trainData)-2
  names(vartypes) <- names( trainData[2:length(trainData)] )
  
  #this object should store the "factors" found for each variable - saves a bit of time.
  varvals <- list()
  length(vartypes) <- length(trainData)-2
  names(vartypes) <- names( trainData[2:length(trainData)] )
  
  #next object should store a set of hypotheses for each variable (iff binary or categorical).
  varhyps <- list()
  length(varhyps) <- length(trainData) - 2
  names(vartypes) <- names( traindata[2:length(trainData)] )
  
  
  #now, populate "vartypes", "varvals", and "varhyps"
  for (v in 1:length(vartypes)) {
    l <- levels(trainData[v+2]) #start after "result" & "ID"
    #If "levels" param = null, var is either int or num; this rule seems to hold true for all vars in Titanic.
    #NUMERIC:
    if (is.null(l)) {
      vartypes[v] = "num"
      varvals[v] = NULL
      varhyps[v] = NULL
      #BINARY:
    } else if (length(l) == 2) {
      vartypes[v] = "bin"
      #The reason we use "v+2" is to skip the first two cols of traindata (varid & result).
      varvals[v] = levels(trainData[v+2])
      varhyps[v] = lapply(varvals[v], function(x) binDecStump(v, x))
      #CATEGORICAL
    } else if (length(l) < length(trainData)/3) {
      vartypes[v] = "cat"
      varvals[v] = levels(trainData[v+2])
      varhyps[v] = lapply(varvals[v], function(x) catDecStump(v, x))
    } else {
      vartypes[v] = NULL
      varvals[v] = NULL
      varhyps[v] = NULL
    }
  }
  if (is.null(vartypes)) stop("Could not use training data.")  
  
  
  me <- list (
    #define the environment where the list is defined.
    thisEnv = thisEnv,    
    #function to return the environment; example of environments and the 'get' function.
    getEnv = function() {
      return(get("thisEnv", thisEnv))
    }        
  )  
  #define value of the list within the current environment.
  assign('this', me, envir=thisEnv)  
  #set name of class
  class(me) <- append(class(me), "stumpEnv")
  
  
  return(me)
  
}