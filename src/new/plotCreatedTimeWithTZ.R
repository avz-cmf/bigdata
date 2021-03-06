# какие столбцы
#   sold: CreatedDate

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

createdTimeWithTZ <- function(sql)
{
  
  # запрос для sold
  querySold = paste("select sold.CreatedDate, sold.StateOrProvince ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID ", sql, ");", sep = "");
  
  # считываем таблицу
  data.sold <- readTable(querySold);

  if(!checkTable(data.sold))
  {
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.sold))
  {
    # функция обработки таблицы
    data.sold = change.sold(data.sold);
    
    
    plotCreatedTimeWithTZ <- function(sold = data.sold){
      
      created_time = as.numeric(format(strptime(sold$CreatedDate, FormatDate), "%H"));
      
      sold_state = subset(sold, select = c(StateOrProvince));
      
      sold_state = transform(sold_state, delTime = 3);
      
      sold_state$delTime[sold_state$StateOrProvince %in% c("WA","OR","CA","NV")]=0;
      
      sold_state$delTime[sold_state$StateOrProvince %in% c("MT","ID","WY","CO","UT","AZ","NM")]=1;
      
      sold_state$delTime[sold_state$StateOrProvince %in% c("ND","MN","SD","IA","WI","IL","NE","KS","MO","OK","AR","TN",
                                                                           "AL","MS","LA","TX","")]=2;
      
      created_time = ifelse(created_time+sold_state$delTime>23,created_time+sold_state$delTime-24,created_time+sold_state$delTime)

      y = hist(created_time,
                  breaks = seq(-1,23,1),
                  plot = F)$counts;

      x = seq(0,23,1)

      id = seq(1,length(x),1)

      res = data.frame(id, x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotCreatedTimeWithTZ());# вызов функции построения гистограммы
    
  }
}