# какие столбцы
#   publish:  ItemID ProductID add_date
#   sold: ItemID

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца

soldProducts <- function(sql)
{
  # создаем запрос для publish
  queryPublish =paste("select publish.ItemID, publish.ProductID, publish.add_date ",
                      "from ", myDbname, ".publish ",
                      "where ItemID > 0 ", sql, ";", sep = "");
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish ", "where ItemID > 0 ", sql, ");", sep = "");
  
  print(queryPublish)
  print(querySold)
  # считываем таблицу
  data.publish <- readTable(queryPublish);
  data.sold <- readTable(querySold);
  
  if(!checkTable(data.publish) || !checkTable(data.sold))
  {
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.publish) & checkTable(data.sold))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    data.sold = change.sold(data.sold);
    
    
    soldProdud <- function(publish = data.publish,
                           sold = data.sold){
      
      sold_Product = merge(subset(sold, select = c(ItemID)), 
                       subset(publish, select = c(ItemID, ProductID)),
                       by.x = "ItemID", 
                       by.y = "ItemID");
      
      sold_Product = data.frame(table(sold_Product$ProductID))
      names(sold_Product) = c('ProductID', 'count_sold')
      
      x = sold_Product$ProductID
      y = sold_Product$count_sold
      
      id = seq(1,length(x),1)
      
      res = data.frame(id, x, y);
      names(res) = c('id', 'ProductID', 'count_sold')
      return(res);
    }
    
    return(soldProdud());
    
  }
}