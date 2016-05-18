# какие столбцы
#   seller_name, count_sold

getBestSeller<-function()
{

  # создаем запросы
  {
    queryCompetitor =paste("select * ",
                        "from ", myDbname, ".competitor;", sep = "");
  }
  
  {
    res = readTable(queryCompetitor);
  }
  
  {
    res = transform(res, id = 1:nrow(res))
  } 
  
  return(res)
}