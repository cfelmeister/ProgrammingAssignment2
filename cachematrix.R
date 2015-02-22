## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix - will verify that a matrix is square
## and provide the functions for creating matrix inverse
## and caching the solve
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## Make sure matrix is square
        if (nrow(x)!=ncol(x)){
                message("Must use a Square Matrix")
        }else{
                ## create the tools for storing the 
                ## matrix and inverse
                set <- function(y){
                        x <<- y
                        inv <<- NULL
                }
                get <- function() x
                setinv <- function(inverse) inv <<- inverse
                getinv <- function() inv
                list(set = set, get = get, 
                     setinv = setinv, 
                     getinv= getinv)
        }
}


## Write a short comment describing this function
## after a Square invertible matrix has been defined 
## by call to makeCacheMatrix
## a call is made to cacheSolve with the list of
## functions returned by makeCacheMatrix provided as x
##
## if the inverse has not been calculated it will be
## "solved" and then stored by setinv function
## If this has already been done, the Inverse
## will be returned from the cached value rather 
## making a new calculation
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## Calculates only if not already calculated
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Getting Cached Data")
                return(inv)
        }
        ## this code only triggered if inv is NULL
        data <-x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
