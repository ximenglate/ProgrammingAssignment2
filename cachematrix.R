##Assignment 2
##Inverse Matrix Caching
##This function is able to cache large computations
##For example, inverse computation of large dimensional matrix
##In this function, we will take advantage of scoping rules of R language
##and to show how they can be manipulated to preserve state inside of R object.



##This function creates a special "vector", which is really a list containing
##a function to 
##1. set the value of matrix
##2. get the value of matrix
##3. set the value of inverse
##4. get the value of inverse
makeCacheMatrix <- function(x = matrix()) { ##creates a special object that can cache its inverse

  inverse <- NULL ##Internal attribute
  set <- function(y) {
    x <<- y ##Use <<- to assignment value to an object in different environment
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) inverse <<- inverseMatrix 
  getInverse <- function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##This function calculates the inverse of matrix created with the above function
cacheSolve <- function(x, ...) { ##computes the inverse of the matrix returned by makeCacheMatrix()

  inverse <- x$getInverse()
  if (!is.null(inverse)) { ##Check if inverse matrix has already been calculated
    message("getting cached data")
    return(inverse)
  }
  
  dataMatrix <- x$get() ##Calculate if cache is empty
  inverse <- solve(dataMatrix, ...)
  x$setInverse(inverse)
  inverse
}


##Now let's have a simple test
set.seed(10000)
N <- 999 ##Set matrix dimension, large enough to show power of caching
mat <- matrix(rnorm(N*N, mean=10, sd=5), nrow=N, ncol=N) ##Create N*N random matrix
temp <- makeCacheMatrix(mat)

##First trial
start = Sys.time()
cacheSolve(temp)
diff1 = Sys.time() - start
print(diff1)
  
##Second trial
start = Sys.time()
cacheSolve(temp)
diff2 = Sys.time() - start
print(diff2)

##Using caching, the execution time of the second trial is significantly reduced.

