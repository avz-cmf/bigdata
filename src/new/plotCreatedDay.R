# какие столбцы
#   sold: CreatedDate

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

createdDay <- function(sql)
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
  }# если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.sold))
  {
    # функция обработки таблицы
    data.sold = change.sold(data.sold);
    
    
    plotCreatedDay <- function(sold = data.sold){
      
      sold_created = as.numeric(format(strptime(sold$CreatedDate, FormatDate), "%u"));
      y = hist(sold_created,
                  breaks = seq(0,7,1),
                  plot = F)$counts; 
      x = seq(1,7,1);
      id = seq(1,length(x),1)
      res = data.frame(id, x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotCreatedDay());# вызов функции построения гистограммы
    
  }
}