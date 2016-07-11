#   sold:  ItemID
#   publish:  ItemID ProductID price_real shipping_real
#   products:  ProductID, ebaycategory_id


tableProduct <- function(sql)
{
  
  
  # создаем запрос
  queryPublish =paste("select publish.ItemID,publish.ProductID, publish.price_real, publish.shipping_real ",
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
    return(NULL);
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда рисуем гистограмму
  if(checkTable(data.publish) & checkTable(data.sold))
  {
    # функция обработки таблицы
    data.publish = change.publish(data.publish);
    data.sold = change.sold(data.sold);
    
    tableOfProduct <-function(publish = data.publish, 
                                    sold = data.sold,
                                    delta_prob = 1.26){
      
      sold_product = data.frame(table(merge(subset(sold, select = c(ItemID)),
                                            subset(publish, select = c(ItemID, ProductID)),
                                            by.x = "ItemID",
                                            by.y = "ItemID")$ProductID));
      names(sold_product) = c("ProductID", "count_sold");
      
      
      push_product = data.frame(table(publish$ProductID));
      names(push_product) = c("ProductID", "count_push");
      
      table_product = merge(sold_product,
                            push_product,
                            by.x = "ProductID",
                            by.y = "ProductID");
      
      {
        mean_price = data.frame(aggregate(publish$price_real+publish$shipping_real,
                                          by = list(ProductID = publish$ProductID),
                                          mean));
        names(mean_price) = c("ProductID", "price")
        
        table_product = merge(table_product,
                              mean_price,
                              by.x = "ProductID",
                              by.y = "ProductID");
      }
      
      
      table_product = transform(table_product, prob = pmin(1,count_sold/count_push));
      
      table_product = transform(table_product, prof_mounth = (prob*price/10-0.05)*count_push/7);
      
      table_product = transform(table_product, new_prob = pmin(1, count_sold*delta_prob/(count_push+(1-delta_prob)*count_sold)));
      
      table_product = transform(table_product, new_prof_mounth = (new_prob*price/10-0.15)*(count_push+(1-delta_prob)*count_sold)/7);
      
      table_product = transform(table_product, delta_prof_mounth = new_prof_mounth - prof_mounth);
      
      table_product = table_product[order(-table_product$delta_prof_mounth),];
      
      table_product = transform(table_product, id = 1:nrow(table_product))
      
      return(table_product)
    }
    return(tableOfProduct()) 
  }
}