linearise_traits <- function(traits){
  normed_traits <- norm_traits(traits)
  lined_traits <- atan(normed_traits[,2]/normed_traits[,1])
  return(lined_traits)
}

trait_matrix_sym <- function(A,Dim,mode_svd) {
  library(Matrix)
  A <- as(A, "sparseMatrix") 
  S <- switch(mode_svd,
              base = base::svd(A,nu = Dim, nv = 0),
              irlba = irlba::irlba(A, nu = Dim, nv = 0),
              RSpectra = RSpectra::svds(A, nu = Dim, k = Dim, nv = 0))
  Traits <- S$u[,1:Dim] %*% diag(sqrt(S$d[1:Dim]),Dim)
  if(!is.null(row.names(A))) row.names(Traits) <- row.names(A)
  return(Traits)
}