## The 2 following R-functions provide a solution to cache the inverse of a matrix. 

## makeCacheMatrix creates a special matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize cache for inverse
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset cache when matrix is changed
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse
    getInverse <- function() inv
    
    # Return a list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve computes the inverse of a matrix defined by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves the cached result.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if inverse is already cached
    
    if (!is.null(inv)) {  # If cached inverse exists, return it
        message("getting cached data")
        return(inv)
    }
    
    # Compute inverse if not cached
    mat <- x$get()
    inv <- solve(mat, ...)  # Compute inverse using solve()
    
    x$setInverse(inv)  # Cache the computed inverse
    
    inv  # Return the inverse
}

## Running them
# Create a 2x2 matrix
A <- matrix(c(4, 7, 2, 6), nrow = 2, ncol = 2)
# Create a cache matrix object
cachedMatrix <- makeCacheMatrix(A)
# Compute the inverse (first time - calculation occurs)
invA <- cacheSolve(cachedMatrix)
print(invA)
# Compute again (retrieved from cache)
invA_cached <- cacheSolve(cachedMatrix)
print(invA_cached)

# Verify that the 2 matrices are inverses
AxinvA <- A %*% invA
invAxA <- invA %*% A
print(AxinvA)
print(invAxA)

# Check if they are identity matrices
all(diag(2) == AxinvA)  # Check if A %*% invA is an identity matrix
all(diag(2) == invAxA)  # Check if invA %*% A is an identity matrix
