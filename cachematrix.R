---

## Put comments here that give an overall description of what your functions do

## Comment 1
## Closures get their name because they enclose the environment of the parent function and can access all its variables. 
## mX and inverseMX are stored in the environment in which they were defined.
## That means the enclosing environment of the get set functions returned by makeCacheMatrix(). 

## Comment 2
## Calculation of matrix inversion is memory intensive. 
## It is therefore a good idea to cache the results of such calculation and have them available for further use.

---

install.packages("Matrix")
library(Matrix)
## solve() function is in the Matrix package
## https://www.rdocumentation.org/packages/Matrix/versions/0.3-26/topics/solve.Matrix

---

## Write a short comment describing makeCacheMatrix() function

## Comment 3
## The return value of this function is a list of the four functions:
## 1. setMatrix()
## 2. getMatrix()
## 3. setInverseMatrix()
## 4. getInverseMatrix()

makeCacheMatrix <- function(mX = matrix()) {
  
  inverseMX <- NULL
  
  setMatrix <- function(mX1) {
    mX <<- mX1
    inverseMX <<- NULL
  }
  
  getMatrix <- function() {
    mX
  }
  
  setInverseMatrix <- function(inverseMX1){ 
    inverseMX <<- inverseMX1
  }
  
  getInverseMatrix = function() {
    inverseMX
  }
  
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix, 
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}  

---
  
## Write a short comment describing cacheSolve() function

## Comment 4
## The return value of this function is a matrix inverse to the input matrix. 
## If the inverse matrix has been already calculated the function will get it from cache and skip a computation.
## If the inverse matrix has not been calculated, the function will calculate ans set the value in the cache.

## cacheSolve() function return value - inverse of the input matrix

cacheSolve <- function(mX, ...) {
  
  ## checking if the inverse matrix has been already calculated and is available in cache
  ## if yes, return the existing matrix
  inverseMX = mX$getInverseMatrix()
  if(!is.null(inverseMX)){
    return(inverseMX)
  }
  
  ## inverse matrix is not available in cache
  ## calculate the inverse matrix
  else{		
    inverseMX = solve(mX$getMatrix(), ...)
  } 
  
  ## set the value of the inverse matrix in cache
  mX$setInverseMatrix(inverseMX)
    
  return(inverseMX)
}

---

## Testing:
testMatrix1 <- matrix(rnorm(1:9), 3)
testMatrix1
cacheStatus <- makeCacheMatrix(testMatrix1)
testMatrix2 <- cacheSolve(cacheStatus)
testMatrix2

## Conclusion: Test passed
## The End
