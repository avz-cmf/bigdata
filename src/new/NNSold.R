#   myParse:  price, title, start_time


plotNN <- function(sql)
{
  
  # создаем запрос
  queryData = paste("select price, sold, shipping",
                    "from ", myDbname, ".myParse",
                    "where ", sql, ";", sep = "");
  
  parse <- readTable(queryData);
  
  maxPrice = log10(max(parse$price+parse$shipping,na.rm = T))+0.2;
  myBreaks = seq(0, maxPrice, by = 0.1);
  
  NN <-function(data = parse)
  {
    
    parse = data.frame(sold =c(1,2,3), price =c(2,3,4), shipping = c(1,1,1))
    
    sold = c()
    
    for (i in 1:(length(parse)))
    {
  
      if (parse$sold[i]!=0)
      {
        for (j in 1:(parse$sold[i]))
        {
          sold = c(sold, parse$price[i]+parse$shipping[i])
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
  
  return(tablePublish()) 
}