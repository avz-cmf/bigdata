# config csv file
#   1. name -имя пользователя
#   2. password - пароль к базе
#   3. host - host базы данных
#   4. bdName - имя базы данных
#   5. route - путь куда сохранять картинки
#   6. myDir - текущая директория



{
  FormatDate = "%Y-%m-%d %H:%M";
  myBDUser = as.character(config[1,1]);
  myBDPassword = as.character(config[1,2]);
  myHost = as.character(config[1,3]);
  myDbname = as.character(config[1,4]);
  myRoute = as.character(config[1,5]);
  myCol = "red";
  mySize = c(960, 960);
}#  считываес параметры с config


library(RMySQL)

readTable <- function(query){
  con <- dbConnect(MySQL(),
                   user = myBDUser,
                   password = myBDPassword,
                   host = myHost,
                   dbname=myDbname);
  
  res <- data.frame(dbGetQuery(conn = con, statement = query));
  
  q<-dbDisconnect(con);
  return(res);
  
}#  функция для считывания таблицы из базы по завросу


checkTable <- function(data){
  return(nrow(data)>10)
  
}# функция для проверки наличия нежного количества данных


change.publish <-function(publish=data.publish){
  
  return(publish);
}# функция для обработки таблицыpublish


change.sold <-function(sold=data.sold){
  
  return(sold);
}# функция для обработки таблицы sold
