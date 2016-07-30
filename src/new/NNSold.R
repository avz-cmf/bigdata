#   myParse:  price, title, start_time


plotNNSold <- function(sql)
{
  
  # создаем запрос
  queryData = paste("select price, sold, shipping ",
                    "from ", 'dima_parser', ".myParse ",
                    "where ind = 1 ", sql, ";", sep = "");
  
  parse <- readTable(queryData, 'dima_parser');
  
  if(nrow(parse)==0){
    return(NULL)
  }

  maxPrice = log10(max(parse$price+parse$shipping,na.rm = T))+0.2;
  myBreaks = seq(-0.1, maxPrice, by = 0.1);
  
  NNSold <-function(data = parse)
  {
    
    sold = c()
    
    for (i in 1:(length(data)))
    {
  
      if (data$sold[i]!=0)
      {
        for (j in 1:(data$sold[i]))
        {
          sold = c(sold, data$price[i]+data$shipping[i])
        }
      }
    }
    
    y = hist(log10(sold),
             breaks = myBreaks, 
             plot = F)$counts;
    
    x = seq(0.05, maxPrice+0.1,0.1)[1:(length(myBreaks)-1)]
    
    id = seq(1,length(x),1)
    
    res = data.frame(id, x, y);
    
    return(res)
  }
  
  return(NNSold()) 
}
