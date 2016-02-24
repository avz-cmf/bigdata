
{
  
  FormatDate = "%Y-%m-%d %H:%M";
  myBDUser = as.character(config[1,1]);
  myBDPassword = as.character(config[1,2]);
  myHost = as.character(config[1,3]);
  myDbname = as.character(config[1,4]);
  myRoute = as.character(config[1,5]);
  myCol = "red";
  mySize = c(960, 960);
}  


library(RMySQL)

readTable <- function(query)
{
  con <- dbConnect(MySQL(),
                   user = myBDUser,
                   password = myBDPassword,
                   host = myHost,
                   dbname=myDbname);
  
  res <- data.frame(dbGetQuery(conn = con, statement = query));
  
  q<-dbDisconnect(con);
  return(res);
  
}#считывание данных из базы данных по запросу

checkTable <- function(data)
{
  return(nrow(data)>10)
  
}# функция проверки наличия данных

change.publish <-function(publish=data.publish){
  
  return(publish);
}

change.sold <-function(sold=data.sold){
  
  return(sold);
}
