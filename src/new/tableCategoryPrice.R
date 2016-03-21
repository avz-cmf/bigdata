#   sold:  ItemID
#   publish:  ItemID ProductID price_real shipping_real
#   products:  ProductID, ebaycategory_id


tableCategoryPrice <- function(sql)
{
  
  
  # создаем запрос
  queryPublish =paste("select publish.ItemID, publish.ProductID, publish.price_real, publish.shipping_real ",
                      "from ", myDbname, ".publish", ", ", myDbname, ".products ",
                      "where publish.ProductID=products.ProductID", sql, ";", sep = "");
  
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID", sql, ");", sep = "");
  
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
 
    tableOfCategoryPrice <-function(publish = data.publish, 
                               sold = data.sold,
                               delta_prob = 1.26){
      
      sold_table = data.frame(table(transform(merge(subset(sold, select = c(ItemID)),
                                                    subset(publish,select = c(ItemID, price_real, shipping_real)),
                                                    by.x = "ItemID",
                                                    by.y = "ItemID"),
                                              category_price = trunc((price_real+shipping_real)/10))$category_price));
      names(sold_table) = c("category_price", "count_sold");
      
      
      push_table = data.frame(table(transform(subset(publish,select = c(price_real,shipping_real)),
                                              category_price = trunc((price_real+shipping_real)/10))$category_price));
      names(push_table) = c("category_price", "count_push");
      
      
      table_category_price = merge(sold_table,
                                   push_table,
                                   by.y = "category_price",
                                   by.x = "category_price");
      
      table_category_price = transform(table_category_price, prob = pmin(1,count_sold/count_push));
      
      table_category_price = transform(table_category_price, prof_mounth = (prob*as.numeric(category_price)-0.05)*count_push/7);
      
      table_category_price = transform(table_category_price, new_prob = pmin(1,count_sold*delta_prob/(count_push+(1-delta_prob)*count_sold)));
      
      table_category_price = transform(table_category_price, new_prof_mounth = (new_prob*as.numeric(category_price)-0.15)*(count_push+(1-delta_prob)*count_sold)/7);
      
      table_category_price = transform(table_category_price, delta_prof_mounth = new_prof_mounth - prof_mounth);
      
      table_category_price = table_category_price[order(-table_category_price$delta_prof_mounth),];
      
      table_category_price = transform(table_category_price, id = 1:nrow(table_category_price))
      
      return(table_category_price);
      
      }
    return(tableOfCategoryPrice()) 
  }
}