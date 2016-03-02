# какие столбцы
#   publish:  ItemID ProductID add_date
#   sold: ItemID

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

soldTime <- function(brand,CategoryID,begDate,endDate)
{
  # создаем запрос для publish
  queryPublish =paste("select publish.ItemID, publish.add_date ",
                      "from ", myDbname, ".publish", ", ", myDbname, ".products ",
                      "where publish.ProductID=products.ProductID", brand, CategoryID,begDate, endDate, ";", sep = "");
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID", brand, CategoryID, 
                    begDate, endDate, ");", sep = "");
  
  # считываем таблицу
  data.publish <- readTable(queryPublish);
  data.sold <- readTable(querySold);
  
  if(!checkTable(data.publish) || !checkTable(data.sold))
  {
    return("ERROR");
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.publish) & checkTable(data.sold))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    data.sold = change.sold(data.sold);
    
    
    plotSoldTime <- function(publish = data.publish,
                             sold = data.sold){
      
      sold_time = merge(subset(sold, select = c(ItemID)), 
                       subset(publish, select = c(ItemID, add_date)),
                       by.x = "ItemID", 
                       by.y = "ItemID");
      
      y = hist(as.numeric(format(strptime(sold_time$add_date, FormatDate), "%H")),
                  breaks = seq(-1,23,1),
                  plot = F)$counts;
      x = seq(0, 23, 1)
      res = data.frame(x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotSoldTime());# вызов функции построения гистограммы
    
  }
}