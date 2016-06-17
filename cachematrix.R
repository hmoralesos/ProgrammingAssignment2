## Programming Assignment 2: Lexical Scoping
## This program compute the inverse of a matrix. For example:
## Let be
## (a<-matrix(1:4,2,2))
## then the matrix inverse of "a" is the matrix b
## x<-makeCacheMatrix(a)
## (b<-cacheSolve(x))

## makeCacheMatrix: This function creates a special "matrix" object that can 
##                  cache its inverse.

makeCacheMatrix<- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
##             returned by makeCacheMatrix above. 

cacheSolve<- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i<-solve(data, ...)
    x$setinverse(i)
    ## Return a matrix that is the inverse of 'x'
    i  
}

