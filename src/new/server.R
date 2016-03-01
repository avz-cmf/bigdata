# stopDaemonizedServer(server)

library(httpuv)
library(Rook)
library(jsonlite)


# считываес параметры с консоля
args <- commandArgs(trailingOnly = T);

# считываем путь у config
confFile = "C:\\Users\\Dima Guk\\Documents\\bigdata\\res\\";

# считываем config
config <- read.table(paste(confFile,"config.csv",sep = ""), sep = ",",header = T);

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
      body = paste('<h1>',toJSON(returnData),'</h1>')
    )
  }
)

server <- startDaemonizedServer("0.0.0.0", 9454, app)

