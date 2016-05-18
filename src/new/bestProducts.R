# какие столбцы
#   title, count_sold

getBestProduct<-function()
{
  
  # создаем запросы
  {
    queryProduct =paste("select * ",
                           "from ", myDbname, ".best_product;", sep = "");
  }
  
  {
    res = readTable(queryProduct);
  }
  
  {
    res = transform(res, id = 1:nrow(res))
  } 
  
  return(res)
}