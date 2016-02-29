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
  
  
  plotPublishPrice <- function(publish = data.publish, 
                          colHist = myCol,
                          waySave = myRoute,
                          size = mySize){
    
    #png(file=paste0(waySave, "plotPublishPrice.png"),width = size[1], height = size[2]);
    
    
    res2 = hist(log10(publish$price_real+publish$shipping_real),
         freq = FALSE,
         plot = F,
         breaks = myBreaks, 
         col = colHist,
         labels = TRUE,
         main = "Гистограмма цены выставленых товаров",
         xlab = "Log10(Price)")$counts;
    
    #lines(density(log10(publish$price_real+publish$shipping_real)),
    #      col="blue",
    #      lwd=2);
    
    #box();
    
    #dev.off(); 
    res = data.frame(res2)
    return(res);
  }#  функция постороения гистограммы которая возвращает имя 
  
  return(plotPublishPrice());# вызов функции построения гистограммы
  
}

}