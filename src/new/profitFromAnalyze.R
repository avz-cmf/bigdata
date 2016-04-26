# какие столбцы
#   publish:  ItemID add_date price_rocky
#   sold: ItemID

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца

profFromPrice <- function(price)
{
  return(ifelse(price<=6,
         1+price*0.15,
         ifelse(price<=20,
                1+price*0.15,
                ifelse(price<=70,
                       2+price*0.1,
                       ifelse(price<=200,
                              5+price*0.06,
                              ifelse(price<=370,
                                     7+price*0.05,
                                     7+price*0.05))))))
  
}

profAnalyze <- function(sql)
{
  
  # создаем запрос для publish
  queryPublish =paste("select publish.ItemID, publish.add_date, publish.price_rocky ",
                      "from ", myDbname, ".publish ",
                      "where ", sql, ";", sep = "");
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold;", sep = "");
  
  # считываем таблицу
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
    
    profHowPublish <- function(publish = data.publish,
                     sold = data.sold){
      
      sumProbWas = 0.01770455;
      
      publish = subset(publish, select = c(ItemID, add_date, price_rocky))
      
      publish = transform(publish, weekDay = as.numeric(format(strptime(add_date, FormatDate), "%u")))
      
      publish = transform(publish, hour = as.numeric(format(strptime(add_date, FormatDate), "%H")))
      
      publish = transform(publish, id = (weekDay-1)*24+hour)
      
      sold = subset(sold, select = c(ItemID));
      sold = aggregate(sold$ItemID, list(ItemID = sold$ItemID), length)
      names(sold)[2] = 'sold';
      
      data = merge(publish,
                   sold,
                   by = "ItemID",
                   all.x = TRUE)
      
      data$sold[is.na(data$sold)]=0
      
      pub = aggregate(data$sold, list(id = data$id),length)
      sol = aggregate(data$sold, list(id = data$id),sum)
      
      res = merge(pub,sol,by = "id")
      names(res) = c("id", "count_publish", "count_sold")
      
    
      
      sumProbNew = sum()
      
      profit = sum(data$sold*profFromPrice(data$price_rocky))-0.05*sum(res$count_publish)
      
      
      rm(list = c('data', 'pub', 'publish','sold','sol'));
      return((sumProbNew/sumProbWas)-1)
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(profHowPublish());  # вызов функции построения гистограммы
    
  }
}


















































