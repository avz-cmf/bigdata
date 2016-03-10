library(httpuv)
library(Rook)
library(jsonlite)


# считываес параметры с консоля
args <- commandArgs(trailingOnly = T);

# считываем путь у config
#confFile = args[1];

confFile = "C:\\Users\\Admin\\Documents\\dima_res\\config.csv";

# считываем config
config <- read.table(paste(confFile,"config.csv",sep = ""), sep = ";",header = T);

# считываем текущую директорию с config
myDir = as.character(config[1,6]);

# устанавливаем директорию
setwd(myDir);

# запускаем скрипт с функциями считывания таблиц
source("readData.R");

# скрипты которые возвращают данные для построения графиков
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

# функция которая определят какой скрипт запускать по названию и возвращает нужные данные

getData <- function(name,brand,CategoryID,begDate,endDate)
{
  if(name == "plotPublishPrice") 
  {
    return(publishPrice(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotSoldPrice")
  {
    return(soldPrice(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotProbPrice")
  {
    return(probPrice(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotProfPrice")
  {
    return(profPrice(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotPublishDay")
  {
    return(publishDay(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotSoldDay")
  {
    return(soldDay(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotProbDay")
  {
    return(probDay(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotCreatedDay")
  {
    return(createdDay(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotPublishTime")
  {
    return(publishTime(brand,CategoryID, begDate, endDate))
  }
  if(name == "plotSoldTime")
  {
    return(soldTime(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotProbTime")
  {
    return(probTime(brand,CategoryID,begDate, endDate))
  }
  if(name == "plotCreatedTime")
  {
    return(createdTime(brand,CategoryID, begDate,endDate))
  }
  if(name == "tableCategory")
  {
    return(tableCategory(brand,CategoryID, begDate, endDate))
  }
  if(name == "tableCategoryPrice")
  {
    return(tableCategoryPrice(brand,CategoryID,begDate,endDate))
  }
  if(name == "tableProduct")
  {
    return(tableProduct(brand,CategoryID,begDate,endDate))
  }
  if(name == "plotCreatedTimeWithTZ")
  {
    return(createdTimeWithTZ(brand, CategoryID, begDate, endDate));
  }
  if(name == "tableModel")
  {
    return(tableModel())
  }
  
}


.lastMessage <- NULL

app <- list(
  call = function(req) {
    wsUrl = paste(sep='',
                  '"',
                  "ws://",
                  ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                  '"')
    reqq=Request$new(req)
    name = reqq$GET()[1];
    brand = reqq$GET()[2];
    CategoryID = reqq$GET()[3];
    begDate = reqq$GET()[4];
    endDate = reqq$GET()[5];
    
    
    begDate = ifelse(begDate!="NA", paste(" and publish.add_date > '", begDate,"'",sep=""),"");
    endDate = ifelse(endDate!="NA", paste(" and publish.add_date < '", endDate,"'",sep=""),"");
    CategoryID = ifelse(CategoryID!="NA", paste(" and products.ebaycategory_id = ", CategoryID, sep = ""), "");
    brand = ifelse(brand!="NA", paste( " and products.brand = ",brand, sep = ""), "");
    
    returnData = getData(name,brand,CategoryID,begDate,endDate)
    
    list(
      status = 200L,
      headers = list(
        'Content-Type' = 'text/json'
      ),
      body = paste(toJSON(returnData))
    )
  }
)


server <- startDaemonizedServer("0.0.0.0", 9454, app);

#server <- startDaemonizedServer("127.0.0.1", 9999, app);
#print(server);
#service(timeoutMs = ifelse(interactive(), 100, 1000));

#while (TRUE) {
#  service()
#  Sys.sleep(0.002)
#}
# stopDaemonizedServer(server)