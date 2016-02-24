
# какие столбцы
#     publish:  ItemID, ProductID, price_real, shipping_real
#     sold:  ItemID
#     product_vehicle:   ProductID, vehicle_id
#     vehicles:  id, vehicle

# параметры которые нужно передавать
#    пользыватель базы данных myBDUser = "admin";
#    пароль к базе данных myBDPassword = "password";
#    myHost = "RDS Host";
#    myDbname = "MyDB";
#	   route = "/var/www/html/img/";
#    myProb = 1.26
#    countView = 100

args <- commandArgs(trailingOnly = T); # Получаем аргументы из командной строки

{
  FormatDate = "%Y-%m-%d %H:%M";
  myBDUser = args[1];
  myBDPassword = args[2];
  myHost = args[3];
  myDbname = args[4];
  myRoute = "/var/www/TestR/csv/";
  myProb = as.integer(args[5]);
  countView = as.integer(args[6]);
  
}# Считывание параметров

{
library(RMySQL)
con <- dbConnect(MySQL(),
                 user = myBDUser,
                 password = myBDPassword,
                 host = myHost,
                 dbname=myDbname);

data.publish <- data.frame(dbGetQuery(conn = con, statement = paste("select ItemID, ProductID, price_real, shipping_real, from ", myDbname, ".publish;", sep = "")));
data.sold <-data.frame(dbGetQuery(conn = con, statement = paste("select ItemID from ", myDbname, ".sold;", sep = "")));
data.product_vehicle <- data.frame(dbGetQuery(conn = con, statement = paste("select ProductID, vehicle_id from ", myDbname, ".product_vehicle;", sep = "")));
data.vehicles <- data.frame(dbGetQuery(conn = con, statement =paste("select id, vehicle from ", myDbname, ".vehicles;", sep = "")));

dbDisconnect(con);

}# считывание с базы данных

{
change.product_vehicle <-function(product_vehicle = data.product_vehicle){
  
  
  return(product_vehicle);
}


change.publish <-function(publish=data.publish){
  
  return(publish);
}

change.vehicles <-function(vehicles=data.vehicles){
  
  return(vehicles)
}
change.sold <-function(sold=data.sold){
  
  return(sold);
}

}#процедуры для обработки считаных таблиц

{
data.product_vehicle = change.product_vehicle();
data.publish = change.publish();
data.sold = change.sold();
data.vehicles = change.vehicles();

}# вызов процедур обработки считаных таблиц

vehSumPrice <- function(sold = data.sold,
                        publish = data.publish,
                        vehicle = data.product_vehicle,
                        vehicles = data.vehicles){
  
  soldAndID = merge(subset(sold, select = c(ItemID)),
                    subset(publish, select = c(ItemID, ProductID, price_real, shipping_real)),
                    by.x = "ItemID",
                    by.y = "ItemID");
  
  sumPriceID = aggregate(soldAndID$price_real + soldAndID$shipping_real,
                         by = list(ProductID = soldAndID$ProductID),
                         sum)
  names(sumPriceID)[2] = "sumPrice";
  
  vehicleSumPrice = merge(subset(vehicle, select = c(ProductID, vehicle_id)),
                          sumPriceID,
                          by.x = "ProductID",
                          by.y = "ProductID"
  );
  vehicleSumProf = aggregate(vehicleSumPrice$sumPrice,
                             by = list(vehicle_id = vehicleSumPrice$vehicle_id),
                             sum);
  names(vehicleSumProf)[2] = "sumPriceSold";
  
  vehicleSumProf = merge(vehicleSumProf,
                         vehicles,
                         by.x = "vehicle_id",
                         by.y = "id");
  
  vehicleSumProf = vehicleSumProf[order(-vehicleSumProf$sumPriceSold),];
  
  return(vehicleSumProf);
  
}#таблица в которой для каждой марки посчитана 
                                                      #сума цен всех проданых товарав которые подходят даной марке



vehCountSold <- function(sold = data.sold,
                         publish = data.publish,
                         vehicle = data.product_vehicle,
                         vehicles = data.vehicles){
  
  soldAndID = merge(subset(sold, select = c(ItemID)),
                    subset(publish, select = c(ItemID, ProductID)),
                    by.x = "ItemID",
                    by.y = "ItemID");
  
  countSoldID = aggregate(soldAndID$ProductID,
                          by = list(ProductID = soldAndID$ProductID),
                          length)
  names(countSoldID)[2] = "countSold";
  
  vehicleCountSold = merge(subset(vehicle, select = c(ProductID, vehicle_id)),
                           countSoldID,
                           by.x = "ProductID",
                           by.y = "ProductID");
  
  vehicleCountSold = aggregate(vehicleCountSold$countSold,
                               by = list(vehicle_id = vehicleCountSold$vehicle_id),
                               sum);
  
  names(vehicleCountSold)[2] = "countSold";
  
  vehicleCountSold = merge(vehicleCountSold,
                           vehicles,
                           by.x = "vehicle_id",
                           by.y = "id");
  
  vehicleCountSold = vehicleCountSold[order(-vehicleCountSold$countSold),]
  
  return(vehicleCountSold);
}#таблица в коротой для каждой марки посчитано 
                                                       #количество всех проданых товаров которые подходят даной марке


vehCountPublish <- function(publish = data.publish,
                            vehicle = data.product_vehicle,
                            vehicles = data.vehicles){
  
  countPublishID = aggregate(publish$ProductID,
                             by = list(ProductID = data.publish$ProductID),
                             length);
  names(countPublishID)[2] = "countPublish";
  
  vehicleCountPublish = merge(subset(vehicle, select = c(ProductID, vehicle_id)),
                              countPublishID,
                              by.x = "ProductID",
                              by.y = "ProductID");
  
  vehicleCountPublish = aggregate(vehicleCountPublish$countPublish,
                                  by = list(vehicle_id = vehicleCountPublish$vehicle_id),
                                  sum); 
  names(vehicleCountPublish)[2] = "countPublish";
  
  vehicleCountPublish = vehicleCountPublish[order(-vehicleCountPublish$countPublish),]
  
  return(vehicleCountPublish);
  
}#таблица в коротой для каждой марки посчитано 
                                                          #количество всех выставленых товаров которые подходят даной марке


vehProb <- function(){
  
  a = merge(vehCountSold(),
            vehCountPublish(),
            by.x = "vehicle_id",
            by.y = "vehicle_id");
  a = transform(a, prob = countSold/countPublish);
  
  a=a[order(-a$prob),]
  
  return(a);
}#вероятность продажи товара который походит даной марке


saveTable <- function(table,
                      waySave = myRoute,
                      name,
                      sepSave = "|"){
  
  
  write.table(head(data.frame(table[1],round(table[-1], digits = 3)),countView),
              file = paste0(waySave, name, ".csv"),
              sep = sepSave,
              row.names = FALSE);
  
}#функция для сохранения таблицы


result = c("a","b","c");

saveTable(table = vehSumPrice(), name = result[1]);

saveTable(table = vehCountSold(), name = result[2]);

saveTable(table = vehProb(), name = result[3]);

type = "Table";
type

result;

