# какие столбцы
#   seller_name, count_sold

getBestSeller<-function()
{
  {
    queryCompetitor =paste("select * ",
                        "from ", myDbname, ".competitor;", sep = "");
  }# создаем запросы
  
  {
    res = readTable(queryCompetitor);
  }# скачиваем таблицу в которой храниться список продавцов и количество их продаж
  
  {
    res = transform(res, id = 1:nrow(res))
  }#  добавляем столбец id
  
  return(res)
}