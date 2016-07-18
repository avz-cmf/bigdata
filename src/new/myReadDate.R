{
  FormatDate = "%Y-%m-%d %H:%M";

}#  считываес параметры с config


library(RMySQL)
library(DBI)

myDbname = 'dima_db'
readTable <- function(query, name = myDbname){
  
  query = sub('brand', 'products.brand', query)
  query = sub('ebaycategory_id', 'products.category_id_path', query)
  
  
  con <- dbConnect(MySQL(),
                   user = "dima",
                   password = "5nfUNO732",
                   host = "185.128.234.3",
                   dbname=name);
  
  
  res <- data.frame(dbGetQuery(conn = con, statement = query));
  
  q<-dbDisconnect(con);
  return(res);
  
}#  функция для считывания таблицы из базы по запросу


checkTable <- function(data){
  return(nrow(data)>10)
  
}# функция для проверки наличия нежного количества данных


change.publish <-function(publish){
  
  return(publish);
}# функция для обработки таблицыpublish


change.sold <-function(sold){
  
  return(sold);
}# функция для обработки таблицы sold


change.products <-function(products){
  
  return(products)
}# функция для обработки таблицы products


change.product_vehicle <- function(product_vehicle){
  
  return(product_vehicle)
}# функция для обработки таблицы product_vehicle


change.vehicles <- function(vehicles){
  
  return(vehicles)
}# функция для обработки таблицы vehicles


change.view <- function(views){
  return(views)
}#функция обработки таблицы view