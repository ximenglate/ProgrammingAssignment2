# Inverse Matrix

## 0 Introduction
This second programming assignment will require you to write an R function is able to cache potentially time-consuming computations. For example, taking the mean of a numeric vector is typically a fast operation. However, for a very long vector, it may take too long to compute the mean, especially if it has to be computed repeatedly (e.g. in a loop). If the contents of a vector are not changing, it may make sense to cache the value of the mean so that when we need it again, it can be looked up in the cache rather than recomputed. In this Programming Assignment will take advantage of the scoping rules of the R language and how they can be manipulated to preserve state inside of an R object.

## 1 Write Function `makeCacheMatrix` and `cacheSolve`
### 1.1 The function `makeCacheMatrix` creates a special "matrix" object that can cache its inverse. This function creates a special "vector", which is really a list containing a function to
- set the value of matrix
- get the value of matrix
- set the value of inverse
- get the value of inverse

### 1.2 The function `cacheSolve` computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then the `cacheSolve` should retrieve the inverse from the cache.

### 1.3 Create N*N random matrix (possibly invertible) to do simple test. N is set large enough to show the power of caching.

## 2. Function Definition and Simple Test

```r
library(knitr)
opts_chunk$set(echo = TRUE) ##Set environment

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


##Now let's do simple test.
set.seed(9999)
N <- 500 ##Large enough
mat <- matrix(rnorm(N*N, mean=10, sd=5), nrow=N, ncol=N) ##Create N*N random matrix
temp <- makeCacheMatrix(mat) ##Assign mat into temp

##First trial
start <- Sys.time() ##Start time of execution
x1 <- cacheSolve(temp) ##Execution of inverse matrix
diff1 <- Sys.time() - start ##Duration of execution
diff1
```

```
## Time difference of 0.1450942 secs
```

```r
##Second trial
start <- Sys.time()
x2 <- cacheSolve(temp)
```

```
## getting cached data
```

```r
diff2 <- Sys.time() - start
diff2
```

```
## Time difference of 0.002002001 secs
```

The total execution time of first trial is 0.1450942s. It is obvious that if we do not use `cacheSolve`, the execution time of second trial should also be 0.1450942s. However, if we take advantage of caching, the execution time is significantly reduced to 0.002002s.

