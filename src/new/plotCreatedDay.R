# какие столбцы
#   sold: CreatedDate

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

createdDay <- function(brand,CategoryID,begDate,endDate)
{
  
  # запрос для sold
  querySold = paste("select sold.CreatedDate ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID", brand, CategoryID, 
                    begDate, endDate, ");", sep = "");
  
  # считываем таблицу
  data.sold <- readTable(querySold);
  
  if(!checkTable(data.sold))
  {
    return("ERROR");
  }# если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.sold))
  {
    # функция обработки таблицы
    data.sold = change.sold(data.sold);
    
    
    plotCreatedDay <- function(sold = data.sold){
      
      sold_created = as.numeric(format(strptime(sold$CreatedDate, FormatDate), "%u"));
      res2 = hist(sold_created,
                  breaks = seq(0,7,1),
                  plot = F)$counts; 
      
      res = data.frame(res2);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotCreatedDay());# вызов функции построения гистограммы
    
  }
}