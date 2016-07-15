#   sold:  ItemID
#   publish:  ItemID ProductID add_date 



tablePublishTime <- function()
{
  
  # создаем запрос для publish
  queryPublish =paste("select publish.ItemID, publish.ProductID, publish.add_date ",
                      "from ", myDbname, ".publish;", sep = "");
  
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ;", sep = "");
  
  
  data.publish <- readTable(queryPublish);
  data.sold <- readTable(querySold);

  if(!checkTable(data.publish) || !checkTable(data.sold))
  {
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда продолжаем работу
  if(checkTable(data.publish) & checkTable(data.sold))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    data.sold = change.sold(data.sold);
    
    tablePublish <-function(publish = data.publish, 
                            sold = data.sold){
      
      # добавляем день недели в таблицу publish
      publish = transform(publish, weekDay = as.numeric(format(strptime(add_date, FormatDate), "%u"))) 
      # добавляем час выставления в таблицу publish
      publish = transform(publish, hour = as.numeric(format(strptime(add_date, FormatDate), "%H")))
      # добавляем столбец id который будет разный для каждого часа в течении недели
      publish = transform(publish, id = (weekDay-1)*24+hour)
      # добавляем столбец sold который случит индекатором продался ли товар
      sold = transform(sold, sold = 1)
      # обьденяем эти две таблицы
      data = merge(publish,
                   sold,
                   by = "ItemID",
                   all.x = TRUE)
      # заполняем нулями столбец sold для товаров которые не продались
      data$sold[is.na(data$sold)]=0
      # агрегируем таблицу для нахождения количеста выставлений 
      pub = aggregate(data$sold, list(id = data$id),length)
      # агрегируем таблицу для нахождения количеста продаж
      sol = aggregate(data$sold, list(id = data$id),sum)
      # обьеденяем таблицу с количеством выставлений и количеством продаж
      res = merge(pub,sol,by = "id")
      names(res) = c("id", "count_publish", "count_sold")
      # находим вероятность продажи товара в заданый час
      res = transform(res, prob = count_sold/count_publish)
      # находим нужное нам распределение выставлений
      sumProb = sum(res$prob)
      res = transform(res, probPub = prob/sumProb)
      
      # создаем матрицу 24/7
      res = matrix(res$probPub,24,7)
      res = data.frame(res)
      res['hour'] = seq(0,23,1)
      names(res) = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday', 'Hour')
      
      res$id = seq(0,23,1)
      
      return(res)
    }
    return(tablePublish()) 
  }
}