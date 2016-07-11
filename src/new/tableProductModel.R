# какие столбцы
#   publish:  ItemID ProductID
#   sold: ItemID
#   product_vehicle ProductID vehicle_id
#   vehicles id vehicle

tableProductModel<- function(){
  
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
    return(NULL);
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
      #таблица количества проданых товараов которые подходят таким же маркам
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
        
        #обьединяем таблицу product_vehicle и таблицу количества продаж деталей к каждой марке 
        model_sold_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                 model_sold_count,
                                 by = "vehicle_id");
        #считаем количества проданых деталей которые подходят каким же маркам
        model_sold_count = aggregate(model_sold_count$count_sold,by = list(ProductID = model_sold_count$ProductID),sum)
        names(model_sold_count)[2] = "count_model_sold" 
        
      }
      
      #таблица количества выставленых товараов которые подходят таким же маркам
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
        
        #обьединяем таблицу product_vehicle и таблицу количества выставлений деталей к каждой марке 
        model_publish_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                    model_publish_count,
                                    by = "vehicle_id");
        
        model_publish_count = aggregate(model_publish_count$count_publish,by = list(ProductID = model_publish_count$ProductID),sum)
        names(model_publish_count)[2] = "count_model_publish" 
        
      }
      
      #создаем список всех ProductID
      product = data.frame(data.frame(table(product_vehicle$ProductID))$Var1)
      names(product) = "ProductID";
      
      #обьединяем количество продаж и ProductID
      res = merge(product,
                  model_sold_count,
                  by = "ProductID",
                  all.x = TRUE);
      #добавляем количество выставлений
      res = merge(res,
                  model_publish_count,
                  by = "ProductID",
                  all.x = TRUE);
      
      # добавляем вероятность продажи
      res = transform(res, prob = ifelse(count_model_publish!=0,count_model_sold/count_model_publish,0));
      
      res = transform(res, id = 1:nrow(res))
      
      return(res);
    }
    
    return(getTable())
  }
}
