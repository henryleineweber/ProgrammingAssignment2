#This function takes a matrix defined in the global environment and caches for later use. Use this function to set a variable, i.e. 'my_matrix <- makeCacheMatrix(matrix(1:4,2,2) . 
#Cache is the cached value of the matrix set in the function. setmat = sets the matrix; getinv = gets the inverse and passes to cache.

makeCacheMatrix <- function(x = matrix()) {
		 cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setmat <- function(inverse) cache <<- inverse
        getinv <- function() cache
        list(set = set, get = get,
             setmat = setmat,
             getinv = getinv)

}

#This function checks to see if there is a cached value of the inverse matrix and if yes, passes it.

cacheSolve <- function(x, ...) {
        
        cache <- x$getinv()

		if (!is.null(cache)){
                message("retrieving the cache value")
                return(cache)
        }
        
        mat.data = x$get()
        cache = solve(mat.data, ...)
        
        x$setmat(cache)
        
        return(cache)
}
