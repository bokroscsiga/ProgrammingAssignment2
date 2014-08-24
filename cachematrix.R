## I want to acknowledge that the basis of this code is originally from 
## the GitHub repository of Roger D. Peng (rdpeng), moreover that it closely
## follows the example "Caching the Mean of a Vector" given in the description
## of Programming Assignment 2.

## The purpose of the following pair of functions is to save computation time
## when matrix inversion is needed repeteadly. 
## The first function, "makeCachematrix" creats the matrix the inverse of 
## which will be computed. 
## The second function, "cacheSolve" accesses the matrix, 
## i) if the inverse of it has not been computed yet, then "cacheSolve" 
## computes it, stores it, and returns it; if it has already been calculated 
## then "cacheSolve" picks it up and returns the value of the inverse. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL                   # "I" will store the inverse of our matrix
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x         # this function returns the original matrix
    setinv <- function(inv) I <<- inv
    getinv <- function() I
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special matrix returned by 
## "makeCacheMatrix" above. If the inverse has already been calculated, 
## then "cacheSolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { 
    I <- x$getinv()             # accesses the matrix 'x' and gets the inverse
    if(!is.null(I)) {           # if inverse was already computed
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinv(I)                 # store the calculated inverse in x
    I
}

