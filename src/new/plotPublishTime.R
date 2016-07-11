# какие столбцы
#   publish:  ProductID add_date

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров


publishTime <-function(sql)
{
  
  
  # создаем запрос
  queryPublish =paste("select publish.add_date ",
                      "from ", myDbname, ".publish", ", ", myDbname, ".products ",
                      "where publish.ProductID=products.ProductID", sql, ";", sep = "");
  
  # считываем таблицу
  data.publish <- readTable(queryPublish);
  
  
  if(!checkTable(data.publish))
  {
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.publish))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    
    plotPublishTime <- function(publish = data.publish){
      
      publish_time = as.numeric(format(strptime(publish$add_date, FormatDate), "%H"));
      
      y = hist(publish_time,
                  breaks = seq(-1, 23, by = 1),
                  plot = F)$counts;
      x = seq(0,23,1)
      id = seq(1,length(x),1)
      res = data.frame(id, x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotPublishTime());# вызов функции построения гистограммы
    
  }
  
}