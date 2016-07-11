# какие столбцы
#   sold: CreatedDate

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

createdTime <- function(sql)
{
  
  # запрос для sold
  querySold = paste("select sold.CreatedDate ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID", sql, ");", sep = "");
  
  # считываем таблицу
  data.sold <- readTable(querySold);
  
  if(!checkTable(data.sold))
  {
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.sold))
  {
    # функция обработки таблицы
    data.sold = change.sold(data.sold);
    
    
    plotCreatedTime <- function(sold = data.sold){
      
      created_time = as.numeric(format(strptime(sold$CreatedDate, FormatDate), "%H"));
      
      y = hist(created_time,
                  breaks = seq(-1,23,1),
                  plot = F)$counts;
      x = seq(0,23,1)
      id = seq(1,length(x),1)
      res = data.frame(id, x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotCreatedTime());# вызов функции построения гистограммы
    
  }
}