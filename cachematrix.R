##
## Caching of time-consuming computations
## 
## This function is able to cache the (time-consuming) computation
## of an the Inverse of a Matrix by taking advantage of the 
## scoping rules of the R language and how they can be manipulated to 
## preserve state inside of an R object.
##
## makeCacheMatrix: creates a special "matrix" object
##    that can cache its inverse.
## 
## cacheSolve: calculates the inverse Matrix of the special "matrix"
##    returned by `makeCacheMatrix`
##
##
## Remark: 
##    the matrix supplied must be invertible.
## 

## 'makeCacheMatrix' creates a special "matrix" object, which is
##  a list containing a function to
## 
## 1.  set the matrix
## 2.  get the matrix
## 3.  set the inverse matrix
## 4.  get the the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- Y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i 
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
}

##'cacheSolve' calculates the inverse Matrix of the special "matrix"
## created with the function 'makeCacheMatrix' (see above). 
##
## It first checks if the inverse Matrix has already been calculated.
## If so, it `get`s the inverse Matrix from the cache and 
## skips the computation. 
## Otherwise, it calculates the inverse Martix via the 'solve' function and 
## sets the inverse Matrix in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i        
}

## 
## Sample 
##
## a <- matrix(data = c(2,1,5,3), nrow=2, ncol=2)
##
## a
## [,1] [,2]
## [1,]    2    5
## [2,]    1    3
##
## b <- makeCacheMatrix(a)
##
## cacheSolve(b)
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
##
## cacheSolve(b)
## getting cached inverse
## [,1] [,2]
## [1,]    3   -5
## [2,]   -1    2
##


