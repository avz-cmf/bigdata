logicOperator = "and|or"

scalarOperator = "eq|nq|le|ge"

resFirst <- function(rql)
{
  rql = strsplit(rql,"&",fixed = TRUE) 
  
  return(rql[[1]][1])
}

decodRQL <- function(rql)
{
  rql = resFirst(rql);
  res = ""
  index = 1;
  log = c("")
  name = ""
  while(nchar(rql)>1)
  {
    if(isScalar(rql))
    {
      op = makeScalarQuery(rql)
      if(substr(op,0,10)=="scriptName")
      {
        name = substr(op, 12, 1000000);
        rql = substr(rql,nchar(strsplit(rql,"\\)")[[1]][1])+2,100000)
        
      }
      else
      {
        res = paste(res, op, sep = "");
        rql = substr(rql,nchar(strsplit(rql,"\\)")[[1]][1])+2,100000)
        if(substr(rql,1,1)==",")
          res = paste(res, " ", log[index], " ", sep = "")
      }
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
  while(substr(res,nchar(res),nchar(res))==" ")  
    res = substr(res,0,nchar(res)-1)
  if(!is.na(name))
    res = c(res, name)
        
  return(res)
  
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
  if(opr=="le")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                "<=",
                strsplit(buf,",")[[1]][2],
                sep = "");
  }
  if(opr=="ge")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                ">=",
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
