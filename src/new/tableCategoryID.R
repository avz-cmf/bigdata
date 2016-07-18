#   sold:  ItemID
#   publish:  ItemID ProductID price_real shipping_real
#   products:  ProductID, ebaycategory_id


tableCategory <- function(sql)
{
  # создаем запрос
  queryPublish =paste("select publish.ItemID,publish.ProductID, publish.price_real, publish.shipping_real ",
                      "from ", myDbname, ".publish", ", ", myDbname, ".products ",
                      "where publish.ProductID=products.ProductID", sql, ";", sep = "");
  
  
  # запрос для sold
  querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ",
                    "where sold.ItemID in (select publish.ItemID from ",myDbname, ".publish, ", myDbname, ".products where publish.ProductID = products.ProductID", sql, ");", sep = "");
  
  # запрос для product
  queryProducts = paste("select ProductID, ebaycategory_id, brand ",
                       "from ", myDbname, ".products;", sep = "");
  print(queryProducts)
  data.publish <- readTable(queryPublish);
  data.sold <- readTable(querySold);
  data.products <- readTable(queryProducts, change = FALSE)
  
  print("asd")
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
    data.products = change.products(data.products)

  tableOfCategory <-function(publish = data.publish, 
                             sold = data.sold,
                             products = data.products,
                             delta_prob = 1.1674503){
    
    sold_ProductID_price = merge(subset(sold, select = c(ItemID)),
                                 subset(publish, select = c(ItemID, ProductID, price_real, shipping_real)),
                                 by.x = "ItemID",
                                 by.y = "ItemID");

    sold_table = data.frame(table(merge(subset(sold_ProductID_price, select = c(ProductID)),  
                                        subset(products, select = c(ProductID, ebaycategory_id)),
                                        by.x = "ProductID", 
                                        by.y = "ProductID")$ebaycategory_id));
    names(sold_table) = c("ebaycategory_id", "count_sold");
    
    
    push_table = data.frame(table(merge(subset(publish, select = c(ProductID)),
                                        subset(products, select = c(ProductID, ebaycategory_id)),
                                        by.x = "ProductID", 
                                        by.y = "ProductID")$ebaycategory_id));
    names(push_table) = c("ebaycategory_id", "count_push");
    
    table_category = merge(sold_table,
                           push_table,
                           by.x = "ebaycategory_id",
                           by.y = "ebaycategory_id");
    
    {
      mean_price_sold = merge(subset(sold_ProductID_price, select = c(ProductID, price_real, shipping_real)),  
                              subset(products, select = c(ProductID, ebaycategory_id)),
                              by.x = "ProductID", 
                              by.y = "ProductID");
      
      mean_category = data.frame(aggregate(mean_price_sold$price_real + mean_price_sold$shipping_real,
                                           by = list(ebaycategory_id = mean_price_sold$ebaycategory_id),
                                           mean));
      names(mean_category)[2] = "mean_price";
      
      table_category = merge(table_category,
                             mean_category,
                             by.x = "ebaycategory_id",
                             by.y = "ebaycategory_id");
    }
    
    table_category = transform(table_category, prob = pmin(1,count_sold/count_push));
    
    table_category = transform(table_category, prof_mounth = (prob*mean_price/10-0.05)*count_push/7);
    
    table_category = transform(table_category, new_prob = pmin(1,count_sold*delta_prob/(count_push+(delta_prob-1)*count_sold)));
    
    table_category = transform(table_category, new_prof_mounth = (new_prob*mean_price/10-0.15)*(count_push+(delta_prob)*count_sold)/7);
    
    table_category = transform(table_category, delta_prof_mounth = new_prof_mounth - prof_mounth);
    
    table_category = table_category[order(-table_category$delta_prof_mounth),];
    
    table_category = transform(table_category, id = nrow(table_category))
    
    return(table_category);
  }
 return(tableOfCategory()) 
}
}