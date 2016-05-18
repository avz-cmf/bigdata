library(httpuv)
library(jsonlite)
library(DBI)


# считываес параметры с консоля
args <- commandArgs(trailingOnly = T);

# считываем путь у config
confFile = args[1];

#confFile = "C:\\Users\\Admin\\Documents\\dima_res\\config.csv";

# считываем config
config <- read.table(confFile, sep = ";",header = T);

# считываем текущую директорию с config
myDir = as.character(config[1,6]);

# устанавливаем директорию
setwd(myDir);

# запускаем скрипт с функциями считывания таблиц
source("readData.R");

# запускаем скрипт декодер RQL
source("decoder.R")

# скрипты которые возвращают данные для построения графиков или таблицы
{
  source("plotPublishPrice.R")
  source("plotSoldPrice.R")
  source("plotProbPrice.R")
  source("plotProfPrice.R")
  source("plotPublishDay.R")
  source("plotSoldDay.R")
  source("plotProbDay.R")
  source("plotCreatedDay.R")
  source("plotPublishTime.R")
  source("plotSoldTime.R")
  source("plotProbTime.R")
  source("plotCreatedTime.R")
  source("tableCategoryID.R")
  source("tableCategoryPrice.R")
  source("tableProduct.R")
  source("plotCreatedTimeWithTZ.R")
  source("tableModel.R")
  source("tableProductModel.R")
  source("getBrand.R")
  source("getCategory.R")
  source("bestProducts.R")
  source("bestCompetitor.R")
}

# функция которая определят какой скрипт запускать по названию и возвращает нужные данные

getData <- function(name, sql)
{
  if(name == "plotPublishPrice") 
  {
    return(publishPrice(sql))
  }
  if(name == "plotSoldPrice")
  {
    return(soldPrice(sql))
  }
  if(name == "plotProbPrice")
  {
    return(probPrice(sql))
  }
  if(name == "plotProfPrice")
  {
    return(profPrice(sql))
  }
  if(name == "plotPublishDay")
  {
    return(publishDay(sql))
  }
  if(name == "plotSoldDay")
  {
    return(soldDay(sql))
  }
  if(name == "plotProbDay")
  {
    return(probDay(sql))
  }
  if(name == "plotCreatedDay")
  {
    return(createdDay(sql))
  }
  if(name == "plotPublishTime")
  {
    return(publishTime(sql))
  }
  if(name == "plotSoldTime")
  {
    return(soldTime(sql))
  }
  if(name == "plotProbTime")
  {
    return(probTime(sql))
  }
  if(name == "plotCreatedTime")
  {
    return(createdTime(sql))
  }
  if(name == "tableCategory")
  {
    return(tableCategory(sql))
  }
  if(name == "tableCategoryPrice")
  {
    return(tableCategoryPrice(sql))
  }
  if(name == "tableProduct")
  {
    return(tableProduct(sql))
  }
  if(name == "plotCreatedTimeWithTZ")
  {
    return(createdTimeWithTZ(sql));
  }
  if(name == "tableModel")
  {
    return(tableModel())
  }
  if(name == "tableProductModel")
  {
    return(tableProductModel())
  }
  if(name == "getBrand")
  {
    return(tableBrand())
  }
  if(name == "getCategory")
  {
    return(getCategoryName())
  }
  if(name == "bestProducts")
  {
    return(getBestProduct())
  }
  if(name == "bestCompetitor")
  {
    return(getBestSeller())
  }
}


.lastMessage <- NULL

app <- list(
  call = function(req) {
    rql = req$QUERY_STRING
    returnData = "";
    print("new requesr")
    if(rql != "")
    {
      rql = substr(rql,2,100000)
      
      query = decodRQL(rql);
      scriptName = query[2];
      print(scriptName);
      SQL = query[1];
      if(SQL!="")
        SQL = paste(" and", SQL);
      print(SQL);
      
      
      returnData = getData(scriptName, SQL);
      print(returnData);
    }else{
      print("not query")
    }
    list(
      status = 200L,
      headers = list(
        'Content-Type' = 'text/json'
      ),
      body = paste(toJSON(returnData))
    )
  }
)


#server <- startDaemonizedServer("0.0.0.0", 9997, app);

server <- startDaemonizedServer("127.0.0.1", 9999, app);
print(server);
service(timeoutMs = ifelse(interactive(), 100, 1000));

while (TRUE) {
  service()
  Sys.sleep(0.002)
}
# stopDaemonizedServer(server)