
## We can avoid repeated computation for inverse matrix by caching the inverse of a matrix, using makeCacheMatrix and cashSolve

## makeCacheMatrix store a special matrix which can be easily used to cache its inverse matrix later.

makeCacheMatrix <- function(x = matrix()) {
    # default for inversed matrix
    inv <- NULL
    
    # Store the new matrix(y) in cache(x)
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    # Print out the stored matrix
    get <- function() {
        x
    }
    
    # In case there is no stored inverse matrix, then store the inverse matrix of new data in inv
    setInverse <- function(inverse) {
    inv <<- inverse    
    }
    
    # Pring out the stored inverse matrix
    getInverse <- function(){
    inv    
    }
    
    # Return a list of functions (and their consequent matrices)
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    }





## casheSolve computes the inverse of the matrix created by makeCacheMatrix above.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
     # Print out the stored inverse matrix (null or actual one) 
    inv <- x$getInverse()     
    
    # In case there is already stored inverse matrix(inv), print it out with a message
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # In case there is nos stored inverse matrix(inv=NULL), compute the inverse matrix using solve() 
    newdata <- x$get()
    inv <- solve(newdata, ...)
    
        
    #Then cache the computed inverse matrix using setInverse()
    x$setInverse(inv)
    
    #Return the inversed matrix of x
    inv
    }
