# какие столбцы
#   ProductID

getBestSeller<-function()
{
  {
    queryDontPublish = paste("select * ",
                           "from ", myDbname, ".dont_publish;", sep = "");
  }# создаем запросы
  
  {
    res = readTable(queryDontPublish);
  }# скачиваем таблицу в которой храниться список продавцов и количество их продаж
  
  {
    res = transform(res, id = 1:nrow(res))
  }#  добавляем столбец id
  
  return(res)
}