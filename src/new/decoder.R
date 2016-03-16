logicOperator = "AND|OR"

scalarOperator = "eq|nq"

resFirst <- function(rql)
{
  rql = strsplit(rql,"&",fixed = TRUE) 
  
  return(rql[[1]][1])
}

decodRQL <- function(rql)
{
  res = ""
  index = 1;
  log = c("")
  while(rql != "")
  {
    print(rql)
    if(isScalar(rql))
    {
      op = makeScalarQuery(rql)
      res = paste(res, op, sep = "");
      rql = substr(rql,nchar(strsplit(rql,"\\)")[[1]][1])+2,100000)
      if(substr(rql,1,1)==",")
        res = paste(res, " ", log[index], " ", sep = "")
    }
    
    if(isLogic(rql))
    {
      index = index + 1;
      opr = strsplit(rql,"\\(")[[1]][1]
      log[index] = opr;
      res = paste(res, "(", sep = "")
      rql = substr(rql,nchar(opr)+2,100000)
      
    }
    if(substr(rql,1,1)==",")
    {
      rql = substr(rql,2,10000000);
    }
    if(substr(rql,1,1)==")")
    {
      index = index - 1;
      rql = substr(rql,2,10000000);
      res = paste(res, ")", " ", log[index], " ", sep = "")
    }
  }
    
  return(substr(res,0,nchar(res)-2))
  
}

makeScalarQuery <- function(rql)
{
  res = "";
  
  opr = strsplit(rql,"\\(")[[1]][1];
  
  buf = substr(strsplit(rql,"\\)")[[1]][1],nchar(opr)+2,100000);

  if(opr=="eq")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                "=",
                strsplit(buf,",")[[1]][2],
                sep = "");
  }
  if(opr=="nq")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                "!=",
                strsplit(buf,",")[[1]][2],
                sep = "");
  }
  
  return(res)
}

isLogic <- function(opr)
{
  return(regexpr(logicOperator, opr)==1)
}

isScalar <- function(opr)
{
  return(regexpr(scalarOperator, opr)==1) 
}
