## Description of TestCachedMatrix()
## This function tests whether the cached matrix is retrieved correctly by cacheSolve()
## in the case where the cached matrix is not NULL

TestCachedMatrix<-function(){
        ## initialize a matrix
        mymatrix<-matrix(1:4,nrow=2,ncol=2)
        ## sets the list of functions to handle inversed matrix
        mycacheMatrix<-makeCacheMatrix(mymatrix)
        ## test1 : check the value of the initial matrix in cache
        print(mycacheMatrix$getinitialmatrix())
        ## calculate the inversed matrix
        mycacheMatrix$setinversedmatrix()
        ## test2 : check the value of the inversed matrix in cache
        inversed_matrix<-mycacheMatrix$getinversedmatrix()
        print(inversed_matrix)
        ## test3 : check the whether the value of the inversed matrix in cache is correct
        inversed_matrix<-cacheSolve(mycacheMatrix)
        print(inversed_matrix)
        
        if(all(inversed_matrix%*%mymatrix==diag(nrow(mymatrix)))){
                print("succesful test")
        }else{
                print("unsuccesful test")
                print(inversed_matrix%*%mymatrix)
        }
        
}

## Description of TestNotCachedMatrix()
## This function tests whether the cached matrix is retrieved correctly by cacheSolve()
## in the case the cached inversed matrix is NULL

TestNotCachedMatrix<-function(){

        ## initialize a matrix
        mymatrix<-matrix(1:4,nrow=2,ncol=2)
        ## sets the list of functions to handle inversed matrix
        mycacheMatrix<-makeCacheMatrix(mymatrix)
        ## test1 : check the value of the initial matrix in cache
        print(mycacheMatrix$getinitialmatrix())
        ## test2 : check the value of the inversed matrix in cache. It should be NULL.
        inversed_matrix<-mycacheMatrix$getinversedmatrix()
        print(inversed_matrix)
        ## test3 : check the whether the value of the inversed matrix in cache is correct
        inversed_matrix<-cacheSolve(mycacheMatrix)
        print(inversed_matrix)
        if(all(inversed_matrix%*%mymatrix==diag(nrow(mymatrix)))){
                print("succesful test")
        }else{
                print("unsuccesful test")
        }
        
}