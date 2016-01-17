## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(newMat) {
        x    <<- newMat
        invX <<- NULL
    }
    get <- function() { x }
    setInv <- function(newInv) { invX <<- newInv }
    getInv <- function() { invX }
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMat <- x$getInv()
    if (!is.null(invMat)) {
        message("getting cached data")
        return(invMat)
    }
    data <- x$get()
    invMat <- solve(data, ...)
    x$setInv(invMat)
    invMat
}

## Test does inversion followed by matrix multiplication (both sides).
## Relative error (of difference against the unit matrix) is printed out.

cacheSolveTest <- function() {
    matDim <- 100
    testNo <- 0
    tiny <- (.Machine$double.xmin) / (.Machine$double.eps)^3
    while (testNo < 30) {
        x <- matrix(rnorm(matDim^2), nrow=matDim, ncol=matDim)
        if (det(x) > tiny) {
            matObj <- makeCacheMatrix(x)
            matDiff <- diag(matDim) - x %*% cacheSolve(matObj)
            d1 <- norm(matDiff, "F")
            matDiff <- diag(matDim) - cacheSolve(matObj) %*% x
            d2 <- norm(matDiff, "F")
            print(max(d1, d2) / norm(diag(matDim), "F"))
            testNo <- testNo + 1
        }
    }
}

