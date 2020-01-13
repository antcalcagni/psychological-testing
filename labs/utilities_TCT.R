compute_VAR.E = function(X){
  N=NCOL(X)
  r = NROW(X)
  VAR.E = 1/N*sum(r/(r-1)*(1/r*apply(X,2,var))) 
  return(VAR.E)
}

compute_VAR.T = function(X){
  N=NCOL(X)
  r = NROW(X)
  var.y = (1/(N*r^2))*(sum(apply(X,2,sum)^2)-(sum(X)^2/N))
  VAR.T = (N/(N-1))*var.y - (1/r)*compute_VAR.E(X)
  return(VAR.T)
}

convert_R_to_Sigma = function(R,sigmap){
  p = NROW(R)
  if(length(sigmap)==1){sigma0 = rep(sigmap,p)}
  else{sigma0=sigmap}
  Sigma0 = diag(sigma0)%*%R%*%diag(sigma0)
  return(Sigma0)
}

generate_Sigma0 = function(p=2,sigmap=3,alphad=1){
  R = clusterGeneration::rcorrmatrix(d = p,alphad = alphad) 
  Sigma0 = convert_R_to_Sigma(R,sigmap)
  return(Sigma0)
}


