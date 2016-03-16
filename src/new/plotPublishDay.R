# какие столбцы
#   publish:  ProductID add_date

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров


publishDay <-function(sql)
{
  
  
  # создаем запрос
  queryPublish =paste("select publish.add_date ",
                      "from ", myDbname, ".publish", ", ", myDbname, ".products ",
                      "where publish.ProductID=products.ProductID", sql, ";", sep = "");
  
  # считываем таблицу
  data.publish <- readTable(queryPublish);
  
  
  if(!checkTable(data.publish))
  {
    return("ERROR");
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.publish))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    
    plotPublishDay <- function(publish = data.publish){
      
      publish_day = as.numeric(format(strptime(publish$add_date, FormatDate), "%u"));
      
      y = hist(publish_day,
                  breaks = seq(0,7,1),
                  plot = F)$counts;
      x = seq(1,7,1)
      id = seq(1,length(x),1)
      res = data.frame(id, x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotPublishDay());# вызов функции построения гистограммы
    
  }
  
}