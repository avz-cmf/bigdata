library(RMySQL)
library(reshape2)

{
FormatDate = "%Y-%m-%d %H:%M";
}# константы

{
  con <- dbConnect(MySQL(),
                   user = 'dima',
                   password = '123qwe321',
                   host = '192.168.1.103',
                   dbname='mototouc_saascom');
  
  
  sold = data.frame(dbGetQuery(conn = con, statement = 'select * from sold'));
  
  publish = data.frame(dbGetQuery(conn = con, statement = 'select * from publish'));
  publish = publish[publish$add_date>'2015-06-01',]
  product_vehicle = read.table('C:\\Users\\Admin\\Documents\\dima\\data\\product_vehicle.txt',header = T,sep = '|')
  
  product = data.frame(dbGetQuery(conn = con, statement = 'select * from products'));
  
  view = data.frame(dbGetQuery(conn = con, statement = 'select * from views'));
  view = view[view$ViewDate>'2015-06-01',]
}# скачиваем таблицы

{
  view = subset(view, select = c(ItemID, ViewDate))
  view = transform(view, mounth_view = paste(as.numeric(format(strptime(ViewDate, FormatDate), "%m"))+
                                       (as.numeric(format(strptime(ViewDate, FormatDate), "%Y"))-2015)*12, '_view' ,sep=''))
  
  view = merge(view,
               subset(publish, select = c(ItemID, ProductID)),
               by = "ItemID")
  
  count_view = data.frame(table(view$ProductID, view$mounth_view))
  names(count_view) = c("ProductID", "mounth_view", "count_view")
  
  count_view = dcast(count_view,ProductID~mounth_view, value.var = 'count_view')

}# создание таблицы count_view_mounth

{
  sold = subset(sold, select = c(ItemID, CreatedDate))
  sold = transform(sold, sold = 1)
  
  sold = merge(sold,
               subset(publish, select = c(ItemID, ProductID)),
               by = "ItemID")
  
  sold = transform(sold, mounth_sold = paste(as.numeric(format(strptime(CreatedDate, FormatDate), "%m"))+
                                       (as.numeric(format(strptime(CreatedDate, FormatDate), "%Y"))-2015)*12, '_sold' ,sep=''));
  
  count_sold = data.frame(table(sold$ProductID, sold$mounth_sold))
  names(count_sold) = c("ProductID", "mounth_sold", "count_sold")
  
  count_sold = dcast(count_sold,ProductID~mounth_sold, value.var = 'count_sold')
  
}# создание таблицы count_sold_mounth

{
  publish = subset(publish, select = c(add_date, ProductID, price_real, shipping_real,ItemID))
  
  publish = transform(publish, mounth_publish = paste(as.numeric(format(strptime(add_date, FormatDate), "%m"))+
                                                (as.numeric(format(strptime(add_date, FormatDate), "%Y"))-2015)*12, '_publish' ,sep=''));
  
  count_publish = data.frame(table(publish$ProductID,publish$mounth_publish))
  
  names(count_publish) = c("ProductID", "mounth_publish", "count_publish")
  
  count_publish = dcast(count_publish,ProductID~mounth_publish, value.var = 'count_publish')
  
}# создание таблицы count_publish_mounth

{
  count = merge.data.frame(count_publish,count_sold,all.x = T)

  count =merge.data.frame(count, count_view, all.x = T)
  count[is.na(count)]=0
  
  rm(list = c('count_publish', 'count_sold', 'count_view'))
}# обьеденяем count_publish count_sold count_view

{
  count_sold = data.frame(table(sold$ProductID))
  names(count_sold) = c("ProductID", "count_sold")
  
  prod = merge(subset(product, select = c(ProductID, brand, ebaycategory_id)),
               count_sold,
               by = "ProductID",
               all.x=TRUE) 
  prod$count_sold[is.na(prod$count_sold)]=0
  
  brand_mean = aggregate(prod$count_sold, by = list(brand = prod$brand),mean)
  names(brand_mean)[2] = "mean_sold_brand"
  
  brand_sum = aggregate(prod$count_sold, by = list(brand = prod$brand), sum)
  names(brand_sum)[2] = "sum_sold_brand" 
  
  
  category_mean = aggregate(prod$count_sold, by = list(category = prod$ebaycategory_id),mean)
  names(category_mean)[2] = "mean_sold_category"
  
  category_sum = aggregate(prod$count_sold, by = list(category = prod$ebaycategory_id), sum)
  names(category_sum)[2] = "sum_sold_category" 
  
  prod = merge(prod,
               brand_sum,
               by = "brand",
               all.x = TRUE)
  prod = merge(prod,
               brand_mean,
               by = "brand",
               all.x = TRUE)
  
  prod = merge(prod,
               category_sum,
               by.x = 'ebaycategory_id',
               by.y = "category",
               all.x = TRUE)
  prod = merge(prod,
               category_mean,
               by.x = 'ebaycategory_id',
               by.y = "category",
               all.x = TRUE)
  
  
  
  
  
  rm(list = c('count_sold','brand_mean', 'brand_sum', 'category_mean', 'category_sum'))
}# получение данных из таблицы product

{
  {
    table_sold = data.frame(table(sold$ProductID))
    names(table_sold) = c("ProductID","count_sold");
    
    model_sold_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             table_sold,
                             by = "ProductID",all.x = TRUE);
    model_sold_count$count_sold[is.na(model_sold_count$count_sold)] = 0
    
    model_sold_count = aggregate(model_sold_count$count_sold,by = list(vehicle_id = model_sold_count$vehicle_id),sum)
    names(model_sold_count)[2] = "count_sold";
    model_sold_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             model_sold_count,
                             by = "vehicle_id");
    model_sold_count = aggregate(model_sold_count$count_sold,by = list(ProductID = model_sold_count$ProductID),sum)
    names(model_sold_count)[2] = "count_model_sold" 
  }# count_sold
  
  {
    table_publish = data.frame(table(publish$ProductID))
    names(table_publish) = c("ProductID","count_publish");
    
    model_publish_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                table_publish,
                                by = "ProductID",
                                all.x = TRUE)
    model_publish_count$count_publish[is.na(model_publish_count$count_publish)] = 0
    
    model_publish_count = aggregate(model_publish_count$count_publish,by = list(vehicle_id = model_publish_count$vehicle_id),sum)
    names(model_publish_count)[2] = "count_publish";
    model_publish_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                model_publish_count,
                                by = "vehicle_id");
    deb = aggregate(model_publish_count$count_publish,by = list(ProductID = model_publish_count$ProductID),length)
    
    
    model_publish_count = aggregate(model_publish_count$count_publish,by = list(ProductID = model_publish_count$ProductID),sum)
    names(model_publish_count)[2] = "count_model_publish" 
    
    model_publish_count = merge(model_publish_count,
                                deb,
                                by="ProductID")
    names(model_publish_count)[3] = "count_model"
  }# count_publish, count_model
  
  {
    view_productID = merge(subset(view, select = c(ItemID)),
                           subset(publish, select = c(ItemID, ProductID)),
                           by = "ItemID")
    table_view = data.frame(table(view_productID$ProductID));
    names(table_view) = c("ProductID", "count_view");
    
    model_view_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             table_view,
                             by = "ProductID",
                             all.x = TRUE)
    model_view_count$count_view[is.na(model_view_count$count_view)] = 0
    
    model_view_count = aggregate(model_view_count$count_view, list(vehicle_id = model_view_count$vehicle_id),sum)
    names(model_view_count)[2] = "count_view"
    
    
    model_view_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             model_view_count,
                             by = "vehicle_id");
    
    
    model_view_count = aggregate(model_view_count$count_view,by = list(ProductID = model_view_count$ProductID),sum)
    names(model_view_count)[2] = "count_model_view" 
    
  }# count_view
  
  rm(list = c('deb', 'table_publish', 'table_sold', 'table_view', 'view_productID'))
}# получение данных из таблицы product_vehicle

{
  Data = subset(publish, select = c(add_date, ProductID, price_real, shipping_real));
  Data = transform(Data, day_publish = as.numeric(format(strptime(add_date, FormatDate), "%u")));
  Data = transform(Data, hour_publish = as.numeric(format(strptime(add_date, FormatDate), "%H")));
  Data = transform(Data, mounth_publish = as.numeric(format(strptime(add_date, FormatDate), "%m"))+
                                         (as.numeric(format(strptime(add_date, FormatDate), "%Y"))-2015)*12);
  Data = subset(Data, select = c(ProductID, price_real, shipping_real, day_publish, hour_publish, mounth_publish));
  
  Data = merge(Data,
               count,
               all.x = T)
  
  Data = merge(Data,
               subset(prod, select = c(ProductID, sum_sold_brand, mean_sold_brand,sum_sold_category,mean_sold_category)),
               all.x = T)
  
  Data = merge(Data,
               model_publish_count,
               all.x = T)
  
  Data = merge(Data,
               model_sold_count,
               all.x = T)
  
  Data = merge(Data,
               model_view_count,
               all.x = T)
  
  Data[is.na(Data)]=0
  
  rm(list = c('count','model_sold_count', 'model_publish_count', 'model_view_count', 'prod'))
}# создание data.frame для прогнозирования

{
  include_mounth = c(15)
  
  for(i in include_mounth)
  {
    buf = subset(Data[Data$mounth_publish==i,], select = c('ProductID', 'price_real', 'shipping_real', 'day_publish',
                                                        'hour_publish', 'sum_sold_brand', 'mean_sold_brand', 'sum_sold_category',
                                                        'mean_sold_category', 'count_model_publish', 'count_model', 'count_model_sold',
                                                        'count_model_view', paste(i-1,'_sold',sep=''),paste(i-2,'_sold',sep=''),
                                                        paste(i-3,'_sold',sep=''), paste(i-1,'_view',sep=''), paste(i-2,'_view',sep=''),
                                                        paste(i-3,'_view',sep=''), paste(i-1,'_publish',sep=''),paste(i-2,'_publish',sep=''),
                                                        paste(i-3,'_publish',sep=''),paste(i,'_sold',sep='')))
    
    names(buf) = c('ProductID', 'price_real', 'shipping_real', 'day_publish',
                   'hour_publish', 'sum_sold_brand', 'mean_sold_brand', 'sum_sold_category',
                   'mean_sold_category', 'count_model_publish', 'count_model', 'count_model_sold',
                   'count_model_view','sold1','sold2','sold3','view1','view2','view3','publish1','publish2','publish3','y')
    if(i==include_mounth[1])
    {
      X = buf
    }
    if(i!=include_mounth[1])
    {
      X = rbind.data.frame(X,buf)
    }
  }
}# преобразование таблицы Data в формат который подходит для прогнозирования

{
  X = transform(X, sold = sold1+sold2+sold3)
  X = transform(X, publish = publish1+publish2+publish3)
  X = transform(X, view = view1+view2+view3)
  
  X = transform(X, prob_sold = ifelse(publish!=0,sold/publish,0))
  
}# добавление новых признаков


write.csv(X, 'test.csv',row.names = F)


