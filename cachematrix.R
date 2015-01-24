## Programming Assignment Number 2, Lexical Scoping
## Keith Solveson, R Programming, 21 Jan 2015
## https://class.coursera.org/rprog-010

## These two R functions work together to calculate and store a matrix
## and its inverse.  The purpose of this assignment is to understand
## how R objects are scoped and persist.

## A typical use of these functions from the command line would be:
#> m     <- matrix(rnorm(9), 3, 3)   ## Create a numeric matrix, store in m
#> mlist <- makeCacheMatrix(m)  ## Create special matrix list, store in mlist
#> inv1  <- cacheSolve(mlist)   ## Call 1, inverts m, caches then returns inverse
#> inv2  <- cacheSolve(mlist)   ## Call 2, returns cached inverse
# returning cache

## Hereafter function and environment are often abbreviated as fn and env.



## The makeCacheMatrix function (fn) accepts a numeric matrix and returns
## a list containing functions to get and set both the matrix itself as
## well as its inverse.  Assign the returned list to an R object, 
## which can then be provided as an argument to the cacheSolve fn.
makeCacheMatrix <- function(nMat = matrix()) {
  
  ## Accept the incoming numeric matrix, assign it to the symbol nMat,
  ## and persist the nMat-matrix pair in the environment created by the 
  ## 'makeCacheMatrix' function.  
  
  ## Note that the env created by 'makeCacheMatrix' fn will persist
  ## if it is assigned to an object in the calling (usually global) env.
  
  
  ## This fn added to better understand the special vector (really list) 
  ## env.  Idea from Mr. Boxwala's post on our user forum.
  sve <- function() { parent.env(environment()) }  
  
  ## Useful env cmds from calling env with special vector P:
  ## ls(P$sve())     ## List objects in  the makeCacheMatrix Env
  ## P$sve()$lMat    ## Return object lMat from makeCacheMatrix Env
  
  
  
  ## Create symbol nMatInv in the 'makeCacheMatrix' env with value NULL.
  nMatInv <- NULL
  
  ## Create the fn 'setMat'.  When called it will assign a (new) numeric matrix 
  ## y to symbol nMat and set nMatInv to NULL.  nMatInv is NULL'ed as the new 
  ## value of nMat means that nMatInv's value is likely not the inverse of nMat.  
  ## Recall that both nMat and nMatInv are stored in setMat's parent env, which
  ## is the env created by makeCacheMatrix, hence the '<<-' operator.
  setMat <- function(y) { nMat <<- y;  nMatInv <<- NULL }
  
  ## Create the fn 'getMat'.  When called it returns the value of nMat.  Note
  ## that getMat will have to search for nMat, which is in its parent env.
  getMat <- function() { nMat }
  
  ## Create the fn 'setInv'.  When called it will assign the value 'inverse' to
  ## symbol nMatInv.  Recall that nMatInv is stored in setInv's parent env.
  ## Hence the '<<-' operator.
  setInv <- function(inverse) { nMatInv <<- inverse }
  
  ## Create the fn 'getInv'.  When called it returns the value of nMatInv.  Note 
  ## that getInv will have to search until it finds nMatInv in its parent env.
  getInv <- function() { nMatInv }
  
  ## Create an anonymous list with (pointers to?) the five fns created 
  ## above and return said list as the result of the makeCacheMatrix fn.
  list(setMat = setMat, getMat = getMat,
       setInv = setInv, getInv = getInv,
       sve = sve)
  
  ## Note that the values of nMat and nMatInv are not returned.  They persist in
  ## the env created by the makeCacheMatrix fn, if its results are assigned to 
  ## an object in the calling environment.      
}



## The cacheSolve fn accepts the 'special' matrix list object created by the
## makeCacheMatrix fn and returns the inverse of the numeric matrix that 
## persists in that object, either via immediate calculation for its initial
## call or cache retrival if it (cacheSolve) has been called before with 
## the same matrix.
cacheSolve <- function(lMat, ...) {
  ## Accept the matrix list created by the makeCacheMatrix fn as 'lMat'
  
  inv <- lMat$getInv()            ## Use getInv fn to fetch the inverse into inv.
  
  if(!is.null(inv))               ## If inv is not NULL, then ...
  {
    message("returning cache")    ## ... return the inverse, which was 
    return(inv)                   ## cached inside lMat's env.
  }
  else                            ## Otherwise ...
  {
    data <- lMat$getMat()         ## get the numeric matrix from lMat
    inv  <- solve(data, ...)      ## calculate its inverse
    lMat$setInv(inv)              ## store said inverse inside lMat's env
    return(inv)                   ## and return the inverse value.
  }    
}
