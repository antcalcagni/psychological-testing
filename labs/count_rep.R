count_rep = function(x){
  res=0;w=rep(0,length(x))
  x_table=table(as.character(x))
  w[1:length(x_table)] = x_table
  y=sum(w==rep(1,3))
  if(y<length(x)){
    res=1
  }
  return(res)
}