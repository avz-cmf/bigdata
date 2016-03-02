# какие столбцы
#   publish:  ProductID price_real shipping_real

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров


publishPrice <-function(brand,CategoryID,begDate,endDate)
{


# создаем запрос
queryPublish =paste("select publish.ProductID, publish.price_real, publish.shipping_real ",
                 "from ", myDbname, ".publish", ", ", myDbname, ".products ",
                 "where publish.ProductID=products.ProductID", brand, CategoryID,begDate, endDate, ";", sep = "");

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
  
  # созлаем точки на очи X гистограммы
  maxPrice = log10(max(data.publish$price_real+data.publish$shipping_real,na.rm = T))+0.2;
  myBreaks = seq(0, maxPrice, by = 0.1);
  
  
  plotPublishPrice <- function(publish = data.publish){
    
    y = hist(log10(publish$price_real+publish$shipping_real),
         plot = F,
         breaks = myBreaks)$counts;
    
    x = seq(0.05, maxPrice+0.1,0.1)[1:(length(myBreaks)-1)]
    
    id = seq(1,length(x),1)
    res = data.frame(id, x, y);
    return(res);
  }#  функция постороения гистограммы которая возвращает имя 
  
  return(plotPublishPrice());# вызов функции построения гистограммы
  
}

}