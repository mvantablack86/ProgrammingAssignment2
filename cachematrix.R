
## The purpose of this assignment is to write a pair of functions that cache the inverse of a matrix.
## The target method is learning to cache the inverse of a matrix rather than compute it repeatedly.
## This takes advantage of the scoping rules of the R language to reduce computation.

##

## The function makeCacheMatrix creates an environment. 
## The inverse matrix is cached inside the object m, which is 
## unique for each instance that the function is called.
## The output of the function is a list with these 5 defined functions: 
## setmatrix, getmatrix, setinverse, getinverse, getenv.

makeCacheMatrix <- function(x = matrix()) {

## Ex. x <- matrix(rnorm(64),8,8)
## To check cached values: xMat<-makeCacheMatrix(x)  ## This runs function
## parent.env(xMat$getenv())$m  ## This checks for the cached mean
## environment(xMat$getmean)  ## This refers to environment of m

    m <- NULL  ## This assigns a NULL value to a variable within the environment 
    evn <- environment()  ## This saves the environment
    y <- NULL

    setmatrix <- function(y){  ## This sets the matrix value
    x <<- y  ## This caches the matrix and assigns value y from parent environment
    m <<- NULL  ## This searches parent environments for a definition of the variable and sets it to NULL
}

getmatrix <- function() x  ## This retrieves the matrix value that was cached with setmatrix
setinverse <- function(solve) m <<- solve  ## The cached value of the inverse matrix is saved as m
getinverse <- function() m  ## The retrieves the saved value of inverse matrix m derived from setinverse
getenv <- function() environment()

list (setmatrix=setmatrix, getmatrix = getmatrix,  
    setinverse = setinverse,
    getinverse = getinverse,
    getenv = getenv) ## This creates a list of the four functions
}

## The function "cacheSolve" returns the matrix inversion derived from the makeCacheMatrix function.

cacheSolve <- function(xMat= m(), ...) {

    ## Return a matrix that is the inverse of 'x'
    ## Run the function Ex. minv <- cacheSolve(xMat = m)
    ## This compares the matrix to previous output

    m <- xMat$getinverse() ## This retrieves a matrix inversion if it has already been calculated
    if(!is.null(m)){ ## This checks if the function cacheSolve has already been run
    if(xMat$setmatrix() == xMat$getmatrix()) { ## This checks if the matrix has changed
    message("retrieving cached data")

    matrix<-xMat$get()
    m<-solve(matrix, ...)
    xMat$setmatrix(m)
    return(m) 
}

    y <- xMat$getmatrix()  ## This runs the getmatrix function to get the value of the input matrix
    xMat$setmatrix(y)  ## This runs the setmatrix function using the input matrix to cache it
    m <- solve(y, ...)  ## This computes the inversion of the input matrix
    xMat$setinverse(m)  ## This runs the setinverse function to cache the inverse
    m ## And this returns the desired inverse
    }
}