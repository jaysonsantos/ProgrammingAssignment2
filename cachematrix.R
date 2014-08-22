
makeCacheMatrix <- function(x = matrix()) {
    # This will hold already calculated inverse matrix.
    cache <- NULL
    set <- function (y) {
        x <<- y
        # After setting a new value, cache must be erased.
        cache <<- NULL
    }

    get <- function () x

    setInverse <- function(inverse) cache <<- inverse
    getInverse <- function() cache

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Tries to get an inverse matrix and it might return NULL if not calculated yet.
    inverse = x$getInverse()
    if (!is.null(inverse)) {
        message('Matrix inverse already calculated.')
        return(inverse)
    }

    # In case cache is NULL calculate it
    message('Matrix inverse not calculated yet, calculating it now.')
    
    matrixData <- x$get()
    inverseMatrix <- solve(matrixData)
    # After solving an inverse matrix, cache it.
    x$setInverse(inverseMatrix)
    inverseMatrix
}

# Creates a matrix and inverse it to check against cache
matrix1 <- matrix(c(2, 4, 6, 8), 2, 2)
inverse <- solve(matrix1)

cacheableMatrix1 <- makeCacheMatrix(matrix1)
inverseMatrix <- cacheSolve(cacheableMatrix1)

# Just check if values are the same
print(inverseMatrix == inverse)

# Checking if it now comes from cache
inverseMatrix <- cacheSolve(cacheableMatrix1)
print(inverseMatrix == inverse)



