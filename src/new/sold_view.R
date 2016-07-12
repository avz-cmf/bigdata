# какие столбцы
#   sold: ItemID
#   view: ItemID

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца

soldView <- function(sql)
{
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish ", "where ItemID > 0 ", sql, ");", sep = "");
  
  queryView = paste("select view.ItemID ",
                    "from ", myDbname, ".view ",
                    "where view.ItemID in (select publish.ItemID from ", myDbname, ".publish ", "where ItemID > 0 ", sql, ");", sep = "")
  
  # считываем таблицу
  data.sold <- readTable(querySold);
  data.view <- readTable(queryView);
  
  if(!checkTable(data.sold) || !checkTable(data.view))
  {
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.sold) & checkTable(data.view))
  {
    # функция обработки таблицы
    data.sold = change.sold(data.sold);
    data.view = change.view(data.view);
    
    soldView <- function(sold = data.sold,
                         view = data.view){
      
      soldTable = data.frame(table(sold$ItemID))
      names(soldTable) = c('ItemID', 'count_sold')
      
      viewTable = data.frame(table(view$ItemID))
      names(viewTable) = c('ItemID', 'count_sold')
      
      data = merge(soldTable,
                   viewTable,
                   by.x = 'ItemID',
                   by.y = 'ItemID',
                   all = TRUE)
      
      data[is.na(data)] = 0
      
      data['prob']= ifelse(data$count_view == 0, 0, data$count_sold/data$count_view)
     
      data$id = seq(1,length(data$prob),1)
      
      return(data);
    }
    
    return(soldView());
    
  }
}