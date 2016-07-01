#   myParse:  price, start_time, sold


sumProfitEbay <- function(sql)
{
  
  # создаем запрос
  queryData = paste("select price, sold",
                    "from ", myDbname, ".myParse",
                    "where ", sql, ";", sep = "");
  
  parse <- readTable(queryData);
  
  NN <-function(data = parse)
  {
    sumProfit = sum(data$sold*data$price)
    
    countSold = sum(data$sold)
    
    res = data.frame(sumProfit, countSold);
    
    return(res)
  }
  
  return(tablePublish()) 
}