# какие столбцы
#   publish:  ProductID price_real shipping_real

# параметры которые нужно передавать
#   1. путь у config
#   2. процент наценки
#   3. дата начала
#	  4. дата конца
#   5. категирия товаров
#   6. бренд товаров

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
  myProf =as.integer(args[2]);
  begDate = args[3];
  endDate = args[4];
  CategoryID = ifelse(args[5]!="NA", args[5], "*");
  brand = ifelse(args[6]!="NA", args[6], "*");
}# считываем остальные параметры с консоля

# запускаем скрипт с функциями считывания таблиц
source("readData.R");

# создаем запрос
queryPublish =paste("select publish.ProductID, publish.price_real, publish.shipping_real ",
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
                          prof = myProf,
                          size = mySize){
    
    png(file=paste0(waySave, paste0("plotPublishPrice", ".png")),width = size[1], height = size[2]);
    
    
    data.push_price_hist = hist(log10(publish$price_real+publish$shipping_real),
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
  
  plotPublish();# вызов функции построения гистограммы
  
}#Р ВµРЎРѓР В»Р С‘ Р Т‘Р В°Р Р…Р Р…РЎвЂ№РЎвЂ¦ Р Т‘Р С•РЎРѓРЎвЂљР В°РЎвЂљР С•РЎвЂЎР Р…Р С• РЎвЂљР С•Р С–Р Т‘Р В° РЎРѓРЎвЂљРЎР‚Р С•Р С Р С–Р С‘РЎРѓРЎвЂљР С•Р С–РЎР‚Р В°Р СР СРЎвЂ№
