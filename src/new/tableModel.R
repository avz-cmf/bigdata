# какие столбцы
#   publish:  ItemID ProductID
#   sold: ItemID
#   product_vehicle ProductID vehicle_id
#   vehicles id vehicle

tableModel<- function(){
  
  # создаем запросы
  {
    queryPublish =paste("select publish.ItemID, publish.ProductID ",
                      "from ", myDbname, ".publish;", sep = "");
  
    querySold = paste("select sold.ItemID ",
                    "from ", myDbname, ".sold ", sep = "");
  
    queryProductVehicle = paste("select product_vehicle.ProductID, product_vehicle.vehicle_id ",
                              "from ",myDbname, ".product_vehicle;", sep = "");
  
    queryVehicles = paste("select vehicles.id, vehicles.vehicle ",
                        "from ", myDbname, ".vehicles;", sep = "");
  }
  
  # считываем таблицы
  {
    Publish <- readTable(queryPublish);
    Sold <- readTable(querySold);
    Product_vehicle <- readTable(queryProductVehicle);
    Vehicles <- readTable(queryVehicles)
  }
  
  if(!checkTable(Publish) || !checkTable(Sold) || !checkTable(Product_vehicle) || !checkTable(Vehicles))
  {
    return("ERROR");
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда выполняем скрипт дальше
  if(checkTable(Publish) & checkTable(Sold) & checkTable(Product_vehicle) & checkTable(Vehicles))
  {
    # функция обработки таблицы
    {
      Publish = change.publish(Publish);
      Sold = change.sold(Sold);
      Product_vehicle = change.product_vehicle(Product_vehicle);
      Vehicles = change.vehicles(Vehicles)
    }
    
    getTable <- function(sold=Sold,
                         publish = Publish,
                         product_vehicle = Product_vehicle,
                         vehicles = Vehicles){
      
      {
      #таблица проданных товаров и ProductID  
      sold_productID = merge(sold,
                             subset(publish,select = c(ItemID, ProductID)),
                             by = "ItemID");
      
      #таблица количества продаж каждого из товаров 
      table_sold = data.frame(table(sold_productID$ProductID))
      names(table_sold) = c("ProductID","count_sold");
      
      #обьединяем таблицу product_vehicle и таблицу количества продаж каждого товара
      model_sold_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                 table_sold,
                                 by = "ProductID",all.x = TRUE);
      #заполним пропущенные значения нулями
      model_sold_count$count_sold[is.na(model_sold_count$count_sold)] = 0
      
      #считаем количество продаж товаров которые подходят каждой из марок  
      model_sold_count = aggregate(model_sold_count$count_sold,by = list(vehicle_id = model_sold_count$vehicle_id),sum)
      names(model_sold_count)[2] = "count_sold";
      }#количество проданых товаров которые подходят заданной модели
      
      {
        #таблица количества выставлений каждого из товаров
        table_publish = data.frame(table(publish$ProductID))
        names(table_publish) = c("ProductID","count_publish");
        
        #обьединяем таблицу product_vehicle и таблицу количества выставлений каждого товара
        model_publish_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                    table_publish,
                                    by = "ProductID",
                                    all.x = TRUE)
        #заполним пропущенные значения нулями
        model_publish_count$count_publish[is.na(model_publish_count$count_publish)] = 0
        
        #считаем количество выставлений товаров которые подходят каждой из марок  
        model_publish_count = aggregate(model_publish_count$count_publish,by = list(vehicle_id = model_publish_count$vehicle_id),sum)
        names(model_publish_count)[2] = "count_publish";
        
      }#количество выставленыз товаров которые подходят заданной модели
      
      #обьеденяем количество продаж и таблицу с индексацией марок
      res = merge(vehicles,
                  model_sold_count,
                  by.x = "id",
                  by.y = "vehicle_id",
                  all.x = TRUE);
      res$count_sold[is.na(res$count_sold)] = 0
      
      #добавляем количество выставленых товаров которые подходят данной модели
      res = merge(res,
                  model_publish_count,
                  by.x = "id",
                  by.y = "vehicle_id",
                  all.x = TRUE);
      res$count_publish[is.na(res$count_publish)] = 0
      
      namaes(res)[1] <- "vehicle_id";
      res = transform(res, id = 1:nrow(res))
      res = transform(res,id=1:nrow(res))
      
      return(res); 
    }
    
    return(getTable())
  }
}
