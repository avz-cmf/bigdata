# какие столбцы
#   publish:  ItemID ProductID price_real shipping_real
#   sold: ItemID

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

# считываес параметры с консоля
args <- commandArgs(trailingOnly = T);

# считываем путь у config
confFile = args[1];

# считываем config
config <- read.table(confFile, sep = ",",header = T);

# считываем текущую директорию с config
myDir = as.character(config[1,6]);

# устанавливаем директорию
setwd(myDir);


{
  begDate = args[2];
  endDate = args[3];
  CategoryID = ifelse(args[4]!="NA", args[4], "*");
  brand = ifelse(args[5]!="NA", args[5], "*");
}# считываем остальные параметры с консоля

# запускаем скрипт с функциями считывания таблиц
source("readData.R");

# создаем запрос для publish
queryPublish = paste("select publish.ItemID, publish.price_real, publish.shipping_real ",
                     "from ", bdName, ".publish", ", ", bdName, ".products ",
                     "where publish.ProductID=products.ProductID and products.brand = '", myBrand, "' and products.ebaycategory_id = ", category, 
                     "and publish.add_date >",begDate, " and publish.add_date < ", endDate, ";", sep = "");
# запрос для sold
querySold = paste("select sold.ItemID ",
                  "from ", bdName, ".sold ",
                  "where sold.ItemID in (select publish.ItemID from ",bdName, ".publish, ", bdName, ".products where publish.ProductID = products.ProductID and products.brand = '", myBrand,"' and products.ebaycategory_id = ", category, 
                  "and publish.add_date >",begDate, " and publish.add_date < ", endDate, ");", sep = "");

# считываем таблицу
data.publish <- readTable(queryPublish);
data.sold <- readTable(querySold);

if(!checkTable(data.publish) || !checkTable(data.sold))
{
  print("ERROR");
}#  если в таблице не достаточно элементов тогда пишем ERROR

# если достаточно элементов тогда рисуем гистограмму
if(checkData(data.publish) & checkTable(data.sold))
{
  # функция обработки таблицы
  data.publish = change.publish();
  data.sold = change.sold();
  
  # созлаем точки на очи X гистограммы
  maxPrice = log10(max(data.publish$price_real+data.publish$shipping_real,na.rm = T))+0.2;
  myBreaks = seq(0, maxPrice, by = 0.1);
  
  
  plotProbPrice <- function(publish = data.publish,
                            sold = data.sold,
                            waySave = myRoute,
                            size = mySize){
    
    soldPrice = merge(subset(sold, select = c(ItemID)), 
                      subset(publish, select = c(ItemID, price_real, shipping_real)),
                      by.x = "ItemID", 
                      by.y = "ItemID");
    
    sold_hist_count = hist(log10(data.sold_and_price$price_real+data.sold_and_price$shipping_real),
                           breaks = myBreaks,
                           plot = F)$counts;
    
    publish_hist_count = hist(log10(publish$price_real+publish$shipping_real),
                              breaks = myBreaks, 
                              plot = F)$counts;
    
    
    prob_count = sold_hist_count/publish_hist_count[1:length(sold_hist_count)];
    
    png(file=paste0(waySave,"plotProbPrice.png"),width = size[1], height = size[2]);
    
    plot(myBreaks[1:length(prob_count)],
         prob_count,
         type = "o", 
         main = "график вероятности продажи товара с заданой ценой",
         xlab = "log10(Price)",
         ylab = "вероятность продажи");
    
    box();
    
    dev.off();
    
    return("plotProbPrice");
  }#  функция постороения гистограммы которая возвращает имя 
  
  plotSoldPrice();# вызов функции построения гистограммы
  
}