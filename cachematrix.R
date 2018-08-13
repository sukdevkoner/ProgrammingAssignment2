## These functions written for Coursera Data Science: R Programming 
## Week 3 Assignment; code is written on Aug 12, 2018
## GitHub username: sukdevkoner

## Write a short comment describing this function
## This function creates a special "matrix" object which can cache its inverse matrix
 makeCacheMatrix <- function(x = matrix()) { ## define the argument with default value as "Matrix"
    invm <- NULL                             ## initialize 'invm' as NULL; its the final inverse matrix 
    set <- function(y) {                    ## define the set function to assign new 
        x <<- y                             ## value of matrix in parent environment
        invm <<- NULL                        ## if there is a new matrix, reset invm to NULL
    }
    get <- function() x                     ## define the get fucntion - returns value of the matrix (argument)
    
    setinverse <- function(inverse) invm <<- inverse  ## assigns value of invm in parent environment
    getinverse <- function() invm                     ## gets the value of invm where called
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## Need this in order to refer 
                                                                                  ## to the functions with the $ operator
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix as above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache and will not re-calculate

 cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinverse()
    if(!is.null(invm)) {
        message("Getting Cached Invertible Matrix")
        return(invm)
    }
    data <- x$get()  # get the matrix defind in makeCacheMatrix function
    invm <- solve(data, ...) # solve function calculates Inverse Matrix
    x$setinverse(invm)
    invm
 }
 
 