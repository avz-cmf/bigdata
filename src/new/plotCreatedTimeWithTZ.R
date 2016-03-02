# какие столбцы
#   sold: CreatedDate

# параметры которые нужно передавать
#   1. путь у config
#   2. дата начала
#	  3. дата конца
#   4. категирия товаров
#   5. бренд товаров

createdTimeWithTZ <- function(brand,CategoryID,begDate,endDate)
{
  
  # запрос для sold
  querySold = paste("select sold.CreatedDate ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID", brand, CategoryID, 
                    begDate, endDate, ");", sep = "");
  
  # считываем таблицу
  data.sold <- readTable(querySold);
  
  if(!checkTable(data.sold))
  {
    return("ERROR");
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.sold))
  {
    # функция обработки таблицы
    data.sold = change.sold(data.sold);
    
    
    plotCreatedTimeWithTZ <- function(sold = data.sold){
      
      created_time = as.numeric(format(strptime(sold$CreatedDate, FormatDate), "%H"));
      
      sold_state = subset(sold, select = c(ItemID, StateOrProvince));
      
      sold_state = transform(sold_state, delTime = 3);
      
      sold_state$delTime[sold_state$StateOrProvince %in% c("WA","OR","CA","NV")]=0;
      
      sold_state$delTime[sold_state$StateOrProvince %in% c("MT","ID","WY","CO","UT","AZ","NM")]=1;
      
      sold_state$delTime[sold_state$StateOrProvince %in% c("ND","MN","SD","IA","WI","IL","NE","KS","MO","OK","AR","TN",
                                                                           "AL","MS","LA","TX","")]=2;
      
      created_time = ifelse(created_time+sold_state>23,created_time+sold_state-24,created_time+sold_state)
      
      y = hist(created_time,
                  breaks = seq(-1,23,1),
                  plot = F)$counts;
      x = seq(0,23,1)
      id = seq(1,length(x),1)
      res = data.frame(id, x, y);
      return(res);
    }#  функция постороения гистограммы которая возвращает имя 
    
    return(plotCreatedTimeWithTZs());# вызов функции построения гистограммы
    
  }
}