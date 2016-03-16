#список логических операций
logicOperator = "and|or"

#список скалярных операций
scalarOperator = "eq|nq|le|ge"

#функция которая возвращает первую часть RQL запроса
resFirst <- function(rql)
{
  rql = strsplit(rql,"&",fixed = TRUE) 
  
  return(rql[[1]][1])
}

#функция парсинга RQL запроса
decodRQL <- function(rql)
{
  #выделяем первую часть
  rql = resFirst(rql);
  res = ""
  index = 1;
  log = c("")
  name = ""
  #запускаем цыкл пока запрос не пустой
  while(nchar(rql)>1)
  {
    #если на очереде скалярный оператор
    if(isScalar(rql))
    {
      #парсим скалятный запрос в SQL
      op = makeScalarQuery(rql)
      
      #если имя в скалярном scriptName тогда не записываем его в SQL а записываем в переменную name
      if(substr(op,0,10)=="scriptName")
      {
        name = substr(op, 12, 1000000);
        rql = substr(rql,nchar(strsplit(rql,"\\)")[[1]][1])+2,100000)
        
      }
      else # если нет тогда записываем в SQL
      {
        res = paste(res, op, sep = "");
        rql = substr(rql,nchar(strsplit(rql,"\\)")[[1]][1])+2,100000)
        if(substr(rql,1,1)==",")
          res = paste(res, " ", log[index], " ", sep = "")
      }
    }
    #если логический оператор
    if(isLogic(rql))
    {
      #добавляем в стек логических операторов текущий оператор
      index = index + 1;
      opr = strsplit(rql,"\\(")[[1]][1]
      log[index] = opr;
      
      res = paste(res, "(", sep = "")
      rql = substr(rql,nchar(opr)+2,100000)
      
    }
    
    #если запятая тогда пропускаем ее
    if(substr(rql,1,1)==",")
    {
      rql = substr(rql,2,10000000);
    }
    
    #если закрывающая скобка тогда вынемаем из стека
    if(substr(rql,1,1)==")")
    {
      index = index - 1;
      rql = substr(rql,2,10000000);
      res = paste(res, ")", " ", log[index], " ", sep = "")
    }
  }
  
  #пока последний пробул тогда удаляем его
  while(substr(res,nchar(res),nchar(res))==" ")  
    res = substr(res,0,nchar(res)-1)
  
  #дописываем в конец name
  res = c(res, name)
        
  return(res)
  
}

# функия обработки скалярных операторов
makeScalarQuery <- function(rql)
{
  res = "";
  
  #определяем прератор
  opr = strsplit(rql,"\\(")[[1]][1];
  
  buf = substr(strsplit(rql,"\\)")[[1]][1],nchar(opr)+2,100000);

  if(opr=="eq")
  {
    if(strsplit(buf,",")[[1]][1]!="scriptName")
    {
      res = paste(res,
                  strsplit(buf,",")[[1]][1],
                  "=","'",
                  strsplit(buf,",")[[1]][2],"'",
                  sep = "");
    }
    else
    {
      res = paste(res,
                  strsplit(buf,",")[[1]][1],
                  "=",
                  strsplit(buf,",")[[1]][2],
                  sep = "");
    }
  }
  if(opr=="nq")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                "!=","'",
                strsplit(buf,",")[[1]][2],"'",
                sep = "");
  }
  if(opr=="le")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                "<=","'",
                strsplit(buf,",")[[1]][2],"'",
                sep = "");
  }
  if(opr=="ge")
  {
    res = paste(res,
                strsplit(buf,",")[[1]][1],
                ">=","'",
                strsplit(buf,",")[[1]][2],"'",
                sep = "");
  }
  return(res)
}

#функция проверки является ли оператор логическим
isLogic <- function(opr)
{
  return(regexpr(logicOperator, opr)==1)
}

#функия проверки являеться ли функция скалярной
isScalar <- function(opr)
{
  return(regexpr(scalarOperator, opr)==1) 
}
