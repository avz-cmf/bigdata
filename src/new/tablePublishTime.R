#   sold:  ItemID
#   publish:  ItemID ProductID add_date 



tablePublishTime <- function()
{
  
  # создаем запрос
  queryPublish =paste("select publish.ItemID, publish.ProductID, publish.add_date ",
                      "from ", myDbname, ".publish;", sep = "");
  
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ;", sep = "");
  
  
  data.publish <- readTable(queryPublish);
  data.sold <- readTable(querySold);
  
  if(!checkTable(data.publish) || !checkTable(data.sold))
  {
    return("ERROR");
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.publish) & checkTable(data.sold))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    data.sold = change.sold(data.sold);
    
    tablePublish <-function(publish = data.publish, 
                            sold = data.sold){
      
      publish = transform(publish, weekDay = as.numeric(format(strptime(add_date, FormatDate), "%u"))) 
      
      publish = transform(publish, hour = as.numeric(format(strptime(add_date, FormatDate), "%H")))
      
      publish = transform(publish, id = (weekDay-1)*24+hour)
      
      sold = subset(sold, select = c(ItemID));
      sold = transform(sold, sold = 1)
      
      data = merge(publish,
                   sold,
                   by = "ItemID",
                   all.x = TRUE)
      
      data$sold[is.na(data$sold)]=0
      
      pub = aggregate(data$sold, list(id = data$id),length)
      sol = aggregate(data$sold, list(id = data$id),sum)
      
      res = merge(pub,sol,by = "id")
      names(res) = c("id", "count_publish", "count_sold")
      
      res = transform(res, prob = count_sold/count_publish)
      
      sumProb = sum(res$prob)
      
      res = transform(res, probPub = prob/sumProb)
      
      res = matrix(res$probPub,24,7)
      
      return(res)
    }
    return(tablePublish()) 
  }
}