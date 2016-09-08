## Define different types of hypotheses. these should all inherit from the same single "hypothesis" class.

# Define the class "decision stump". The below is one way this could work. Make sure that you
#   define separate functions to find the optimal configuration of each type of variable.
# These functions should do the main work of deciding which hypothesis would be best for each
#   variable.

#binary hypotheses: [output 1 -> true, output 2 -> false] or vice versa.
#   these are factors with only two levels.
# Inputs: 1. var: variable that we are focusing on; 2. val: predicted value of that variable.
BinDecStump <- function(Var, Val) {
  var = Var
  val = Val
  thisEnv = environment()
  me <- list (
    #define the environment where the list is defined.
    thisEnv = thisEnv,
    
    #this function should return the prediction for a single case only. Note: this function is
    #  only tailored to cases where you have to predict a binary output.
    getHypPrediction <- function(input) {
      if (input[var] == val) return(1)
      else return(0)
    }
  )
  return(me)
}

#categorical hypotheses: [output m -> true, otherwise false] for all m < n (total categories).
#   also, if total categories is too great, ignore the variable? Or maybe don't?
#   these are factors with more than two levels, or ints / numericals with less than certain threshold.
# Inputs: 1. var: variable we are focusing on; 2. val: value focused on; 3. inv: specifies if we
#    want the variable to be inverted: for example a=> survival vs. not(a) => survival.
CatDecStump <- function(Var, Val, Inv) {
  var = Var
  val = Val
  inv = Inv
  thisEnv = environment()
  me <- list (
    #define the environment where the list is defined.
    thisEnv = thisEnv,
    
    #this function should return the prediction for a single case only.
    #This function should return 1 if the inverse param is 0 and the input value equals the
    #  hypothesis value; or if the inverse param is 1 and the input value is unequal.
    getHypPrediction <- function(input) {
      if (input[var] == val & !inv) return(1)
      else if (input[var] != val & inv) return(1)
      else return(0)
    }
  )
  return(me)
}
  
#continuous variables
#   these are integers / numericals (over a threshold).
#   For now, make these like categorical variables by looking at ranges? Should think about this more.
# Inputs: var: variable to focus in; 2. val: value to focus on; 3. Gtr: survival is gtr. or less than
#    val?
NumDecStump <- function(Var, Val, Gtr) {
  var = Var
  val = Val
  gtr = Gtr
  thisEnv = environment()
  me <- list (
    #define the environment where the list is defined.
    thisEnv = thisEnv,
    
    #this function should return the prediction for a single case only. 
    getHypPrediction <- function(input) {
      if (val > input[var] & gtr) return(1)
      else if (val < input[var] & !gtr) return(1)
      else return(0)
    }
  )
  return(me)
  
}

#------------------------------------------------------------------------------------------------------------#
# !!!!TODO BELOW!!!!: remove null values before calculating error / best hypothesis.
# Create methods associated with decision stumps.

#Evaluate the output of a hypothesis. Maybe the method should differ based on the type of class.
##  maybe evaluate just one row of a data frame - return 1 or 0, or whatever - depending on output type.
getHypPredictions <- function(allData, hyp) {
  #apply the below to all **rows** of the data (frame)
  predictions <- apply(allData, hyp$getHypPrediction)
}
#Given a hypothesis, our training data, and a set of weights, come up with an error value
getDSError <- function(trainData, weights, hyp) {
  #get predictions of all hypotheses, then take a weighted average of all erroneous predictions.
  hypPreds <- sapply(trainData, function(x) getHypPrediction(x, hyp))
  hypErr <- sum(abs(hypPreds-trainData$Result) * weights)
  return(hypErr)
}

#generate a weakly accurate numerical hypothesis by taking the median (maybe first quartile?) of all zeros and ones,
#  and returning the line that goes exactly between them.
#Inputs: training data, and weights for each training data point.
#Output: the point of separation, plus the value of "gtr"
#!!!!!!FUTURE IMPROVEMENT!!!!!!: divide 'survived' weights by smallest weight, and then round. Include this number of values
#  of whatever val the weight belongs to when calculating the median.
getNumDecStump(trainData, trainDataWts, var) {
  survived <- trainData[var][trainData$Result == 1]
  ms <- median(survived)
  nsurvived <- trainData[var][trainData$Result == 0]
  mns <- median(nsurvived)
  val <- 0.5 * ms + 0.5 * mns
  return( NumDecStump(c(var, val, ms>mns)) )
  
}

#Wrapper for the above: takes set of hypotheses and finds the best.
getOptHyp <- function(trainData, trainDataWts, hyps, v) {
  if (is.null(hyps)) {
    #In this case, the variable must be numeric; generate and return a "num" hyp.
    #AR: IMPLEMENT BELOW FUNCTION TO FIND OPTIMAL NUMERIC DECISION STUMP.
    numHyp <- GetNumDecStump(trainData, trainDataWts, v)
    err <- getDSError(numHyp)
    return(c(numHyp, err))
  }
  #case 2: binary or categorical variable
  errvals <- lapply(hyps, function(x) getDSError(trainData, trainDataWts, x))
  
  return(c( hyps[which.min(errvals)], min(errvals) ))
}



#Input: training data, trdata weights, and stump environment (which includes all variable factors,
#   hypotheses, and types).
#Output: return the one hypothesis that most accurately fits the weighted training data. Also
#  returns the error of this hyp and a "whitelist", containing non-erroneous training data points.
#Method: test out all hypotheses; find the one with least error and return it.
getDecisionStump <- function(trainData, trainDataWts, stumpEnvInstance) {
  #search through every variable and return its optimal hypothesis and error.
  minErr = sum(trainDataWts)
  optHyp = NULL
  for (v in 1:length(stumpEnvInstance$vartypes)) {
    #skip this variable if it's not numeric, binary, or categorical.
    if (is.null(stumpEnvInstance$vartypes[v])) {
      next
    }
    #AR: IMPLEMENT BELOW FUNCTION (getOptHyp): takes the training data, weights, and hypotheses & returns opt.
    #  hypothesis, as well as its error.
    hyp_err <- getOptHyp(trainData, trainDataWts, stumpEnvInstance$varhyps[v], v)
    if (hyp_err[2] < minErr) {
      minErr = hyp_err[2]
      optHyp = hyp_err[1]
    }
  }
  #return the optimum hypothesis and error:
  return(c(optHyp, minErr))
}























