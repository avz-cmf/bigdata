# какие столбцы
#   publish:  ProductID price_real shipping_real

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

# создаем запрос
queryPublish =paste("select publish.price_real, publish.shipping_real ",
                 "from ", bdName, ".publish", ", ", bdName, ".products ",
                 "where publish.ProductID=products.ProductID and products.brand = '", myBrand, "' and products.ebaycategory_id = ", category, 
                 "and publish.add_date >",begDate, " and publish.add_date < ", endDate, ";", sep = "");

# считываем таблицу
data.publish <- readTable(queryPublish);


if(!checkTable(data.publish))
{
  print("ERROR");
}#  если в таблице не достаточно элементов тогда пишем ERROR

# если достаточно элементов тогда рисуем гистограмму
if(checkData(data.publish))
{
  # функция обработки таблицы
  data.publish = change.publish();
  
  # созлаем точки на очи X гистограммы
  maxPrice = log10(max(data.publish$price_real+data.publish$shipping_real,na.rm = T))+0.2;
  myBreaks = seq(0, maxPrice, by = 0.1);
  
  
  plotPublishPrice <- function(publish = data.publish, 
                          colHist = myCol,
                          waySave = myRoute,
                          size = mySize){
    
    png(file=paste0(waySave, "plotPublishPrice.png"),width = size[1], height = size[2]);
    
    
    hist(log10(publish$price_real+publish$shipping_real),
         freq = FALSE,
         breaks = myBreaks, 
         col = colHist,
         labels = TRUE,
         main = "Гистограмма цены выставленых товаров",
         xlab = "Log10(Price)");
    
    lines(density(log10(publish$price_real+publish$shipping_real)),
          col="blue",
          lwd=2);
    
    box();
    
    dev.off(); 
    
    return("plotPublishPrice");
  }#  функция постороения гистограммы которая возвращает имя 
  
  plotPublishPrice();# вызов функции построения гистограммы
  
}