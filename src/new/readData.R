# config csv file
#   1. name -имя пользователя
#   2. password - пароль к базе
#   3. host - host базы данных
#   4. bdName - имя базы данных
#   6. myDir - текущая директория



{
  FormatDate = "%Y-%m-%d %H:%M";
  myBDUser = as.character(config[1,1]);
  myBDPassword = as.character(config[1,2]);
  myHost = as.character(config[1,3]);
  myDbname = as.character(config[1,4]);
}#  считываес параметры с config


library(RMySQL)


readTable <- function(query){
  
  query = sub('brand', 'products.brand', query)
  query = sub('ebaycategory_id', 'products.category_id_path', query)
  
  
  con <- dbConnect(MySQL(),
                   user = myBDUser,
                   password = myBDPassword,
                   host = myHost,
                   dbname=myDbname);


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