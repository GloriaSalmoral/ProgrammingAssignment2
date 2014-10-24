## My assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#I set the value of the object as a matrix

makeCacheMatrix <- function(x = matrix()) {
        #I initialize the inverse
        inv <- NULL
        set <- function(y) {
                #<<- assign a value to an object in an environment that is different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        #I get the value of the object
        
        get <- function() x
        #I set the inverse function
        setinverse <- function(inverse) inv <<- inverse
       
        #I get the inverse function
     
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



# The following function returns the inverse of the matrix. 


# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        #It first checks if the inverse has already been computed.
        inv <- x$getinverse()
        # If so, it gets the result and skips the computation. 
        
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        #If not, it computes the inverse, sets the value in the cache via setinverse function.
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
}

## Sample run:
## > x = rbind(c(1, -1/4), c(-1/4, 1))
## > m = makeCacheMatrix(x)
## > m$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(m)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 


