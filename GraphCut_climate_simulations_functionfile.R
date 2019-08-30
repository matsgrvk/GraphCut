mat.pad <- function(mat1){
  na1 <- matrix(NA,ncol=1,nrow=nrow(mat1))
  mat2 <- matrix(c(na1,mat1,na1),nrow=nrow(mat1),ncol=ncol(mat1)+2)
  na2 <- matrix(NA,nrow=1,ncol=ncol(mat2))
  mat3 <- rbind(na2,mat2,na2)
  return(mat3)
}

locate <- function(matrice){
  matrice2 <- mat.pad(matrice)
  x <- 1:length(matrice)
  ind_row <- 2:(nrow(matrice)+1)
  ind_col <- 2:(ncol(matrice)+1)
  neigh = rbind(N  = as.vector(matrice2[ind_row - 1, ind_col    ]),
                E  = as.vector(matrice2[ind_row    , ind_col + 1]),
                S  = as.vector(matrice2[ind_row + 1, ind_col    ]),
                W  = as.vector(matrice2[ind_row    , ind_col - 1]))
  return(neigh[,x])
}

test_matgc <- function(matrice){
  n <- rep(1:length(matrice),each=4)
  x <- rep(1:4, times=length(matrice))
  test_matgc <- matrix(c(n,location[x,n]))
  return(test_matgc)
}

locate_poids <- function(matrice){
  matrice2 <- mat.pad(matrice)
  x <- 1:length(matrice)
  ind_row <- 2:(nrow(matrice)+1)
  ind_col <- 2:(ncol(matrice)+1)
  N  = abs(as.vector(matrice2[ind_row - 1, ind_col    ]) + as.vector(matrice2[ind_row,ind_col]))
  E  = abs(as.vector(matrice2[ind_row    , ind_col + 1]) + as.vector(matrice2[ind_row,ind_col]))
  S  = abs(as.vector(matrice2[ind_row + 1, ind_col    ]) + as.vector(matrice2[ind_row,ind_col]))
  W  = abs(as.vector(matrice2[ind_row    , ind_col - 1]) + as.vector(matrice2[ind_row,ind_col]))
  if (length(N) == 0) N=c(NA,NA)
  if (length(E) == 0) E=c(NA,NA)
  if (length(S) == 0) S=c(NA,NA)
  if (length(W) == 0) W=c(NA,NA)
  neigh =  rbind(N,E,S,W)   
  return(neigh[,x])
}