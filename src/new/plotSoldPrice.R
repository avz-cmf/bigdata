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
config <- read.table(paste(confFile,"config.csv",sep = ""), sep = ",",header = T);

# считываем текущую директорию с config
myDir = as.character(config[1,6]);

# устанавливаем директорию
setwd(myDir);


{
  begDate = ifelse(args[2]!="NA", paste(" and publish.add_date > '", args[2],"'",sep=""),"");
  endDate = ifelse(args[3]!="NA", paste(" and publish.add_date < '", args[3],"'",sep=""),"");
  CategoryID = ifelse(args[4]!="NA", paste(" and products.ebaycategory_id = ", args[4], sep = ""), "");
  brand = ifelse(args[5]!="NA", paste( " and products.brand = ",args[5], sep = ""), "");
}# считываем остальные параметры с консоля

# запускаем скрипт с функциями считывания таблиц
source("readData.R");

# создаем запрос для publish
queryPublish =paste("select publish.ItemID, publish.ProductID, publish.price_real, publish.shipping_real ",
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
  print("ERROR");
}#  если в таблице не достаточно элементов тогда пишем ERROR

# если достаточно элементов тогда рисуем гистограмму
if(checkTable(data.publish) & checkTable(data.sold))
{
  # функция обработки таблицы
  data.publish = change.publish();
  data.sold = change.sold();
  
  # созлаем точки на очи X гистограммы
  maxPrice = log10(max(data.publish$price_real+data.publish$shipping_real,na.rm = T))+0.2;
  myBreaks = seq(0, maxPrice, by = 0.1);
  
  
  plotSoldPrice <- function(publish = data.publish,
                            sold = data.sold,
                            colHist = myCol,
                            waySave = myRoute,
                            size = mySize){
    
    soldPrice = merge(subset(sold, select = c(ItemID)), 
                      subset(publish, select = c(ItemID, price_real, shipping_real)),
                      by.x = "ItemID", 
                      by.y = "ItemID");
    
    png(file=paste0(waySave, "plotSoldPrice.png"),width = size[1], height = size[2]);
    
    
    hist(log10(soldPrice$price_real+soldPrice$shipping_real),
               freq = FALSE,
               breaks = myBreaks, 
               col = colHist,
               labels = TRUE,
               main = "Гистограмма цены проданых товаров",
               xlab = "Log10(Price)");
    
    lines(density(log10(publish$price_real+publish$shipping_real)),
          col="blue",
          lwd=2);
    
    box();
    
    dev.off(); 
    
    return("plotSoldPrice");
  }#  функция постороения гистограммы которая возвращает имя 
  
  plotSoldPrice();# вызов функции построения гистограммы
  
}