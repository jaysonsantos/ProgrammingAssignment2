
makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    set <- function (y) {
        x <<- y
        cache <<- NULL
    }

    get <- function () x

    setInverse <- function(inverse) cache <<- inverse
    getInverse <- function() cache

    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    inverse = x$getInverse()
    if (!is.null(inverse)) {
        message('Matrix inverse already calculated.')
        return(inverse)
    }

    message('Matrix inverse not calculated yet, calculating it now.')
    matrixData <- x$get()
    inverseMatrix <- solve(matrixData)
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



