# какие столбцы
#   title, count_sold

getBestProduct<-function()
{
  {
    queryProduct =paste("select * ",
                           "from ", myDbname, ".best_product;", sep = "");
  }# создаем запросы
  
  {
    res = readTable(queryProduct);
  }# считывем таблицу в которой храниться список самых продаваемых товаров
  
  {
    res = transform(res, id = 1:nrow(res))
  }# добавляем поле id к таблице
  
  return(res)
}