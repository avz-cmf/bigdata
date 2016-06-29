library(DBI)
library(RMySQL)
library(reshape2)

{
FormatDate = "%Y-%m-%d %H:%M";
}# константы

{
  # покдлючаемся к базе в которой лежат таблицы sold, publish, products, views
  con <- dbConnect(MySQL(),
                   user = 'dima',
                   password = '123qwe321',
                   host = '192.168.1.104',
                   dbname = 'dima_db');
  
  # скачиваем таблицу sold
  sold = data.frame(dbGetQuery(conn = con, 
                               statement = 'select * from sold'));
  
  # скачиваем таблицу publish
  publish = data.frame(dbGetQuery(conn = con, 
                                  statement = 'select * from publish'));
  publish = publish[publish$add_date > '2015-06-01', ]
  
  # скачиваем таблицу product_vehicle и отключаемся от базы
  product_vehicle = data.frame(dbGetQuery(conn = con, 
                                          statement = 'select * from product_vehicle'));
  
  # скачиваем таблицу products 
  product = data.frame(dbGetQuery(conn = con, 
                                  statement = 'select * from products'));
  
  # скачиваем таблицу view
  view = data.frame(dbGetQuery(conn = con, 
                               statement = 'select * from views'));
  view = view[view$ViewDate > '2015-06-01', ]
  
  # находим максимальный месяц товаров которые есть в таблице для прогнозирования
  maxMounth = dbGetQuery(conn = con, 
                         statement = 'SELECT max(mounth_publish) FROM predictTable')[1,1]
}# скачиваем таблицы

{
  # оставляем в таблице view только столбцы ItemID, ViewDate
  view = subset(view, 
                select = c(ItemID, ViewDate))
  
  # добавляем столбец который сотвествует месяцу посмотра начиная с 2015 года
  view = transform(view, 
                   mounth_view = paste(as.numeric(format(strptime(ViewDate, FormatDate), "%m"))+
                                       (as.numeric(format(strptime(ViewDate, FormatDate), "%Y"))-2015)*12, '_view' ,sep=''))
  
  # добавляем столбец который соотвествует ProductID товара который просматрели
  view = merge(view,
               subset(publish, select = c(ItemID, ProductID)),
               by = "ItemID")
  
  # создаем таблицу частот с количеством просмотров каждого из товаров в каждий из месяцов
  count_view = data.frame(table(view$ProductID, 
                                view$mounth_view))
  names(count_view) = c("ProductID", "mounth_view", "count_view")
  
  # используя библиотеку reshape2 преобразовываем таблицу смежности в нужный нам формат
  count_view = dcast(count_view,
                     ProductID~mounth_view, 
                     value.var = 'count_view')

}# создание таблицы count_view_mounth

{
  # оставляем в таблице sold только столбцы ItemID, CreatedDate
  sold = subset(sold, 
                select = c(ItemID, CreatedDate))
  
  # добавляем столбец из одиниц 
  sold = transform(sold, 
                   sold = 1)
  
  # добавляем ProductID обьеденяя с таблицей publish
  sold = merge(sold,
               subset(publish, select = c(ItemID, ProductID)),
               by = "ItemID")
  
  # добавляем столбец который соотвествует месяцу продажи начиная с 2015
  sold = transform(sold, 
                   mounth_sold = paste(as.numeric(format(strptime(CreatedDate, FormatDate), "%m"))+
                                       (as.numeric(format(strptime(CreatedDate, FormatDate), "%Y"))-2015)*12, '_sold' ,sep=''));
  
  # создаем таблицу частот для каждого товара и месяца
  count_sold = data.frame(table(sold$ProductID, 
                                sold$mounth_sold))
  names(count_sold) = c("ProductID", "mounth_sold", "count_sold")
  
  # преобразуес таблицу в нужный формат
  count_sold = dcast(count_sold,
                     ProductID~mounth_sold, 
                     value.var = 'count_sold')
  
}# создание таблицы count_sold_mounth

{
  #  оставляем в таблице publish только столбцы add_date, ProductID, price_real, shipping_real, ItemID
  publish = subset(publish, 
                   select = c(add_date, ProductID, price_real, shipping_real, ItemID))
  
  # добавляес столбец который соотвествует месяцу выставления товара начиная с 2015
  publish = transform(publish, 
                      mounth_publish = paste(as.numeric(format(strptime(add_date, FormatDate), "%m"))+
                                                (as.numeric(format(strptime(add_date, FormatDate), "%Y"))-2015)*12, '_publish' ,sep=''));
  # создаем таблицу частот для каждого товара и для каждого месяца
  count_publish = data.frame(table(publish$ProductID,
                                   publish$mounth_publish))
  names(count_publish) = c("ProductID", "mounth_publish", "count_publish")
  
  # преобразовываем нашу таблицу в нужный формат
  count_publish = dcast(count_publish,
                        ProductID~mounth_publish, 
                        value.var = 'count_publish')
  
}# создание таблицы count_publish_mounth

{
  # обьеденяем таблицу count_publish и count_sold
  count = merge.data.frame(count_publish,
                           count_sold,
                           all.x = T)
  
  # также добавляем таблицу count_view
  count = merge.data.frame(count, 
                           count_view, 
                           all.x = T)
  count[is.na(count)]=0
  
  # удаляес талицы count_publish, count_sold, count_view
  rm(list = c('count_publish', 'count_sold', 'count_view'))
  
}# обьеденяем count_publish count_sold count_view

{
  # создем талицу частот продаж для каждого товара
  count_sold = data.frame(table(sold$ProductID))
  names(count_sold) = c("ProductID", "count_sold")
  
  # обьеденяем талицу product и таблицу частот продаж каждого товара
  prod = merge(subset(product, select = c(ProductID, brand, ebaycategory_id)),
               count_sold,
               by = "ProductID",
               all.x=TRUE) 
  prod$count_sold[is.na(prod$count_sold)]=0
  
  # создаем таблицу brand_mean в которой для каждой категории будет записана среднее количество проданого товара доного бренда
  brand_mean = aggregate(prod$count_sold, 
                         by = list(brand = prod$brand),
                         mean)
  names(brand_mean)[2] = "mean_sold_brand"
  
  # создаем таблицу с сумарным количество проданых товаров каждого бренда
  brand_sum = aggregate(prod$count_sold, 
                        by = list(brand = prod$brand), 
                        sum)
  names(brand_sum)[2] = "sum_sold_brand" 
  
  # создаем таблицу с средним количеством продаж товара из каждой категории
  category_mean = aggregate(prod$count_sold, 
                            by = list(category = prod$ebaycategory_id),
                            mean)
  names(category_mean)[2] = "mean_sold_category"
  
  # создаем таблицу с сумарным количеством продаж товаров из каждой категории
  category_sum = aggregate(prod$count_sold,
                           by = list(category = prod$ebaycategory_id),
                           sum)
  names(category_sum)[2] = "sum_sold_category" 
  
  # обьеденяем таблицу всех продуктов и сумарного количества продаж товаров бренда
  prod = merge(prod,
               brand_sum,
               by = "brand",
               all.x = TRUE)
  
  # добавляем среднее количество продаж товара соотвествующего бренда
  prod = merge(prod,
               brand_mean,
               by = "brand",
               all.x = TRUE)
  
  # добавляем сумарнное количество продаж соотвествующей категории
  prod = merge(prod,
               category_sum,
               by.x = 'ebaycategory_id',
               by.y = "category",
               all.x = TRUE)
  
  # добавляес среднее количество продаж товара соотвествующей категории
  prod = merge(prod,
               category_mean,
               by.x = 'ebaycategory_id',
               by.y = "category",
               all.x = TRUE)
  
  # удаляем таблицы count_ sold, brand_mean, brand_sum, category_mean, category_sum
  rm(list = c('count_sold','brand_mean', 'brand_sum', 'category_mean', 'category_sum'))
  
}# получение данных из таблицы product

{
  {
    # таблица количества продаж каждого их товаров
    table_sold = data.frame(table(sold$ProductID))
    names(table_sold) = c("ProductID","count_sold");
    
    # добавляем к таблице product_vehicle количество продаж каждого из товаров
    model_sold_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             table_sold,
                             by = "ProductID",all.x = TRUE);
    model_sold_count$count_sold[is.na(model_sold_count$count_sold)] = 0
    
    # таблица с сумарным количеством проданых товаров которые подходят каждой из марок мотоциклов
    model_sold_count = aggregate(model_sold_count$count_sold,by = list(vehicle_id = model_sold_count$vehicle_id),sum)
    names(model_sold_count)[2] = "count_sold";
    
    # обьеденяем таблицу семежности марок и продуктов с таблицей сумарного количества продаж товаров каждой марки 
    model_sold_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             model_sold_count,
                             by = "vehicle_id");
    
    # таблица сумарного количества продынх товаров которые подходят какой же марке 
    model_sold_count = aggregate(model_sold_count$count_sold,by = list(ProductID = model_sold_count$ProductID),sum)
    names(model_sold_count)[2] = "count_model_sold"
    
  }# count_model_sold
  
  {
    # таблица количества выставлений каждого товара
    table_publish = data.frame(table(publish$ProductID))
    names(table_publish) = c("ProductID","count_publish");
    
    # обьеденяем таблицу смежности марок и продуктов с количеством выставлений каждого товара
    model_publish_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                table_publish,
                                by = "ProductID",
                                all.x = TRUE)
    model_publish_count$count_publish[is.na(model_publish_count$count_publish)] = 0
    
    # создаем таблицу количества выставлений товаров каждой марки 
    model_publish_count = aggregate(model_publish_count$count_publish,by = list(vehicle_id = model_publish_count$vehicle_id),sum)
    names(model_publish_count)[2] = "count_publish";
    
    # обьеденяем таблицу смежности марок и продуктов с количеством выставлений товаров каждой марки
    model_publish_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                                model_publish_count,
                                by = "vehicle_id");
    
    # таблица количества марок которые подходят каждому из продуктов
    deb = aggregate(model_publish_count$count_publish,by = list(ProductID = model_publish_count$ProductID),length)
    
    # создаем таблицу количества выставленых товаров которые подходят таким же маркам 
    model_publish_count = aggregate(model_publish_count$count_publish,by = list(ProductID = model_publish_count$ProductID),sum)
    names(model_publish_count)[2] = "count_model_publish" 
    
    # обьеденяем клоичество марок и количество выставленых товаров которые подходят таким же маркам
    model_publish_count = merge(model_publish_count,
                                deb,
                                by="ProductID")
    names(model_publish_count)[3] = "count_model"
    
  }# count_publish, count_model
  
  {
    # создаем таблицу просмотров каждого из товаров
    view_productID = merge(subset(view, select = c(ItemID)),
                           subset(publish, select = c(ItemID, ProductID)),
                           by = "ItemID")
    
    # создаем таблицу частот просмотров каждого товара
    table_view = data.frame(table(view_productID$ProductID));
    names(table_view) = c("ProductID", "count_view");
    
    # обьеденяем таблицу частот просмотров с таблицей смежности марок и продуктов
    model_view_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             table_view,
                             by = "ProductID",
                             all.x = TRUE)
    model_view_count$count_view[is.na(model_view_count$count_view)] = 0
    
    # создаем таблицу количества просмотров товаров которые подходят каждой из марок
    model_view_count = aggregate(model_view_count$count_view, list(vehicle_id = model_view_count$vehicle_id),sum)
    names(model_view_count)[2] = "count_view"
    
    # добавляем количество просмотров товаров которые подходят каждой из марок в таблицу смежности марок и товаров
    model_view_count = merge(subset(product_vehicle, select = c(ProductID, vehicle_id)),
                             model_view_count,
                             by = "vehicle_id");
    
    # создем таблицу количества просмотров товаров которые подходят таким же маркам как и даная
    model_view_count = aggregate(model_view_count$count_view,by = list(ProductID = model_view_count$ProductID),sum)
    names(model_view_count)[2] = "count_model_view" 
    
  }# count_view
  
  # удаляем таблицы deb, table_publish, table_sold, table_view, view_productID
  rm(list = c('deb', 'table_publish', 'table_sold', 'table_view', 'view_productID'))
  
}# получение данных из таблицы product_vehicle

{
  # создаем таблицу с столбцами  add_data, ProductID, price_real, shipping_real
  Data = subset(publish, 
                select = c(add_date, ProductID, price_real, shipping_real));
  
  # добавляем в таблицу столбец месяц выставления начиная с 2015
  Data = transform(Data, 
                   mounth_publish = as.numeric(format(strptime(add_date, FormatDate), "%m"))+
                                         (as.numeric(format(strptime(add_date, FormatDate), "%Y"))-2015)*12);
  # убираем с таблицы столбец add_date
  Data = subset(Data, 
                select = c(ProductID, price_real, shipping_real, mounth_publish));
  
  # обьеженяем нашу таблицу с таблицей количества продаж выставлений просмотров за каджый месяц
  Data = merge(Data,
               count,
               all.x = T)
  
  # обьеденяем нашу таблицу с таблицей сумарного и среднего количества продаж по брендам и категориям
  Data = merge(Data,
               subset(prod, select = c(ProductID, sum_sold_brand, mean_sold_brand,sum_sold_category,mean_sold_category)),
               all.x = T)
  
  # обьеденяем нашу таблицу с таблицуй количества выставленых товаров которые подходят таким же маркам
  Data = merge(Data,
               model_publish_count,
               all.x = T)
  
  # обьеденяем нашу таблицу с таблицей количества проданых товаров которые подходят таким же маркам
  Data = merge(Data,
               model_sold_count,
               all.x = T)
  
  # обьеденяем нашу таблицу с таблицей количества просмотров товаров которые подходят таким же маркам
  Data = merge(Data,
               model_view_count,
               all.x = T)
  
  # заполняем пропущеные значения нулями
  Data[is.na(Data)]=0
  
  # удаляем таблицы count, model_sold_count, mode_publish_count, 'model_view_count, prod
  rm(list = c('count','model_sold_count', 'model_publish_count', 'model_view_count', 'prod'))
  
}# создание data.frame для прогнозирования

# количество месяцов по итогам которых мы будем прогнозировать
k=3

# список номеров месяцов которые нужно включать в данные которые нужно добавить в базу 
include_mounth = sort(unique(Data$mounth_publish)) 
include_mounth = include_mounth[include_mounth>maxMounth]
include_mounth = include_mounth[include_mounth<max(include_mounth)-2]

# если есть данные которые нужно добавить в таблицу
if (length(include_mounth)>0)
{    
  {
    # для каждого месяца данных за который нет в базе добавим все нужные данные в таблицу X
    for(i in include_mounth)
    {
      
      colum = c('ProductID', 'price_real', 'shipping_real', 'mounth_publish', 'sum_sold_brand', 'mean_sold_brand', 'sum_sold_category',
                'mean_sold_category', 'count_model_publish', 'count_model', 'count_model_sold',
                'count_model_view')
      
      for (j in 1:k)
      {
        colum = c(colum, paste(i-j,'_sold',sep=''), paste(i-j,'_publish',sep=''), paste(i-j,'_view',sep=''))
      }
      
      colum = c(colum, paste(i,'_sold',sep=''))
      
    
      buf = subset(Data[Data$mounth_publish==i,], select = colum)
      
      colum_res = c('ProductID', 'price_real', 'shipping_real', 'mounth_publish', 'sum_sold_brand', 'mean_sold_brand', 'sum_sold_category',
                    'mean_sold_category', 'count_model_publish', 'count_model', 'count_model_sold',
                    'count_model_view')
      
      for (j in 1:k)
      {
        colum_res = c(colum_res, paste('sold', j, sep = ''), paste('publish', j, sep = ''), paste('view', j, sep = ''))
      }
      
      colum_res = c(colum_res, 'y')
      
      names(buf) = colum_res
      
      if(i==include_mounth[1])
      {
        X = buf
      }
      if(i!=include_mounth[1])
      {
        X = rbind.data.frame(X,buf)
      }
    }
    
    # удаляем вспомогательную переменную buf
    rm(list = c('buf'))
    
  } # преобразование таблицы Data в формат который подходит для прогнозирования
  
  {
    # списки переменных которые отвичают за количество продаж, выставлений и просмотров за превидущие месяцы
    colum_sold = c('sold1')
    colum_publish = c('publish1')
    colum_view = c('view1')
    for(j in 2:k)
    {
      colum_sold = c(colum_sold, paste('sold', j, sep = ''))
      colum_publish = c(colum_publish, paste('publish', j, sep = ''))
      colum_view = c(colum_view, paste('view', j, sep = ''))
    }
    
    # добавляем столбец который соотвестует сумарному количеству продаж за последние k месяцев
    X = transform(X, sold = rowSums(X[colum_sold]))
    
    # добавляем столбец который соотвествует сумарному количеству выставлений за последние k месяцев
    X = transform(X, publish = rowSums(X[colum_publish]))
    
    # добавляес столбец который соотвествует сумарному количеству просмотров за последние k месяцев
    X = transform(X, view = rowSums(X[colum_view]))
    
    # добавляем столбец который соотвествует веротности продажи товара посчитаная за последние k месяцев
    X = transform(X, prob_sold = ifelse(publish!=0,sold/publish,0))
    
    # добавляем столбец который соотвествует вероятности продажи товара который подходит таким же маркам
    X = transform(X, prob_sold_model = ifelse(count_model!=0, count_model_sold/count_model, 0))
    
    # добавляем столбец который соотвествует вероятности того что товар будет просмотрен если он подходит таким же маркам
    X = transform(X, prob_view_model = ifelse(count_model!=0, count_model_view/count_model, 0))
    
    
  }# добавление новых признаков
  
  # убераем все примеры в которых хотя бы раз не было выставлено товар за последние k месяцев
  X=X[X$publish1>0 & X$publish2>0 & X$publish3>0,]
  
  # записываем нашу таблицу в базу данных
  dbWriteTable(con, "predictTable", value = X, row.names=0, append = TRUE)
  
}

# выходим из базы
dbDisconnect(conn = con)

