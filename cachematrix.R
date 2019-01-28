##Matrix inversion is time consuming so caching the inverse of a matrix rather than computing it repeatedly is helpful.  
##These twofunctions cache the inverse of a matrix.


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCachseMatrix <- function(x = matrix()){
        i < NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <-function() x
        setInverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)
}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x,...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setmean(i)
        i
}