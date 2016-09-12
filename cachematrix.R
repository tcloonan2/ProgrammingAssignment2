## Project Week 3 Assignment
## Student: Tom Cloonan
## School: Coursera 
## Program: Introduction to Data Sciences
## Course: Course 2- R Programming
## Week: 3
############################################
##
## The two functions defined below are called makeCacheMatrix() and cacheSolve(). They work together to input a square, invertible matrix,
## invert it, store a cached version of the inverted matrix, and permit one to determine if that cached version is choherent or not (i.e.- if
## the data in the cached matrix is still useable). Using all of this information, cacheSolve() will return the inverse of the input matrix
## in a very efficient fashion (using the cached value if it is useable).
##
############################################
##
## makeCacheMatrix() will accept as an input a square, invertible matrix. It will define four sub-functions (or methods) that can be called
## by the calling function (cacheSolve())... 
## The first of the sub-functions will be the function called set(), which sets 
## the variable internal variable x with the input matrix.
## The second of the sub-functions will be the function called get(), which reads out
## the current variable x (which holds the input matrix that was last set by set())
## The third of the sub-functions will be the function called setCachedInverse(), which calculates the inverse of the input matrix stored in
## local variable x the "long" way... and stores it in the local variable inverse
## The fourth of the sub-functions will be the function called getCachedInverse(), which returns the cached inverse.
## The cached inverse of the matrix will be NULL to begin with... thus, cacheSolve() needs to check if the returned matrix from getInverse() is NULL
## or if it has changed since the last time the inverse was calculated (using the makeCacheMatrix$getLastInput() sub-function). 

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setCachedInverse <- function(inv) inverse <<- inv
        getCachedInverse <- function() inverse
        list(set = set, get = get,
             setCachedInverse = setCachedInverse,
             getCachedInverse = getCachedInverse)
}



## cacheSolve() will accept as an input a square, invertible matrix. It will then use intelligent use of the sub-functions inside of makeCacheMatrix()
## to obtain the inverted copy of the input matrix and will return that inverted matrix. 
## If the cached matrix is coherent (useable for this query), then it will use the cached matrix (to
## save some processing time). If the cached matrix is not coherent (meaning a new input matrix was used), then it will have to make a call into
## makeCacheMatrix that re-calculates the inverse.
## We assume that the x variable is loaded using the set() function... so when it is, inverse is set to NULL (which is the trigger for not using the cached
## data)

cacheSolve <- function(x, ...) {
        inverse <- x$getCachedInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setCachedInverse(inverse)
        inverse
}


