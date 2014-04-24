## This files contains two functions: a special "matrix" object that can cache its inverse
## and a function that computes an inverse of the special "matrix" and in case the result
## was calculated previously, returns the value from the cache

## The first fuction, makeCacheMatrix creates a special "matrix", containing a list of four 
## functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invM <- NULL ## creates an empty placeholder for a matrix
        set <- function(y) {
                x <<- y
                invM <<-NULL ## setting the value of the matrix for future testing
        }
        get <- function() x ## getting the matrix
        setinverse <- function(inv) invM <<- inv ## setting an inverted matrix values
        getinverse <- function() invM ## getting an inverted matrix
        
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse) ## creates a list with all the functions
}


## The second function, cacheSolve, calculates the inverse of the special "matrix" created with
## the makeCacheMatrix above. The function below first checks if there is already a result for 
## an inverted matrix, and if there is one, returns the result from the cache. In case the
### result is missing, the function performs the calculation and returns the inverse matrix, 
## other than caching the value of this inverse matrix with setinverse

cacheSolve <- function(x, ...) {
        invM <- x$getinverse() ## recovers the inverted matrix for testing
        if(!is.null(invM)) { ## tests if the inverted matrix is already calculated, if yes, returns cache
                message("getting cached data")
                return(invM) ## return cache
        }
        matrix <- x$get() ## loads a normal matrix
        invM <- solve(matrix, ...) ## calculates the inverted matrix
        x$setinverse(invM) ## caches the inverted matrix
        invM ## returns the inverted matrix
}
