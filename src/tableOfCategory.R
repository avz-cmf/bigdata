# какие столбцы
#   sold:  ItemID
#   publish:  ItemID ProductID price_real shipping_real
#   products:  ProductID, ebaycategory_id

# параметры которые нужно передавать
#   пользыватель базы данных myBDUser = "admin";
#   пароль к базе данных myBDPassword = "password";
#   myHost = "RDS Host";
#   myDbname = "MyDB";
#	  route = "/var/www/TestR/csv/";
#   myProb = 1.26
#   countView = 100


# дополнительные параметры

# мин цена
# макс цена
# бренд

args <- commandArgs(trailingOnly = T); # ѕолучаем аргументы из командной строки

{
  FormatDate = "%Y-%m-%d %H:%M";
  myBDUser = args[1];
  myBDPassword = args[2];
  myHost = args[3];
  myDbname = args[4];
  myRoute = "/var/www/TestR/csv/";
  myProb = as.integer(args[5]);
  countView = as.integer(args[6]);
  minPrice = ifelse(args[7]!="NA", args[7], "0");
  maxPrice = ifelse(args[8]!="NA", args[8], "99999");
  brand = ifelse(args[9]!="NA", args[9], "*");
  
}# —читывание параметров

getData <- function(name,
                    bdName = myDbname,
                    myBrand = brand,
                    minP = minPrice,
                    maxP = maxPrice)
{
  if(name == "publish")
    return(paste("select publish.ItemID, publish.ProductID, publish.price_real, publish.shipping_real ",
                 " from ", bdName, ".publish", ", ", bdName, ".products ",
                 "where publish.ProductID=products.ProductID and products.brand = '", myBrand, "' and publish.price_real>", minP, " and publish.price_real<", maxP,";"  , sep = ""));
  
  
  if(name == "sold")
    return(paste("select sold.ItemID ",
                 "from ", bdName, ".sold ",
                 "where sold.ItemID in (select publish.ItemID from ",bdName, ".publish, ", bdName, ".products where publish.ProductID = products.ProductID and products.brand = '", myBrand,"' and publish.price_real>", minP, " and publish.price_real<", maxP, ");", sep = ""));
  
  
  if(name=="products")
  {
    return(paste("select ProductID, ebaycategory_id ",
                 "from ", bdName, "products;"));
  }
  
}


{
library(RMySQL)
con <- dbConnect(MySQL(),
                 user = myBDUser,
                 password = myBDPassword,
                 host = myHost,
                 dbname=myDbname);

data.publish <- data.frame(dbGetQuery(conn = con, statement = getData("publish")));
data.sold <-data.frame(dbGetQuery(conn = con, statement = getData("sold")));
data.products <- data.frame(dbGetQuery(conn = con, statement = getData("products")));

q <- dbDisconnect(con);

}# считывание с базы данных
checkData <- function(data)
{
  return(nrow(data)>10)
}

if(!checkData(data.sold) | !checkData(data.publish) | !checkData(data.products) )
{
  print("ERROR");
}

if(checkData(data.sold) & checkData(data.publish) & checkData(data.products))
{

{

change.products <-function(products=data.products){
  names(products)[1] = "ProductID";
  
  return(products)
}
change.publish <-function(publish=data.publish){
  
  return(publish);
}

change.sold <-function(sold=data.sold){
  
  return(sold);
}

}#процедуры дл€ обработки считаных таблиц


{
data.publish = change.publish();
data.sold = change.sold();
data.products = change.products();
}# вызов процедур обработки считаных таблиц

tableOfCategory <-function(publish = data.publish, 
                           sold = data.sold,
                           products = data.products,
                           delta_prob = myProb){
  
  #таблица всех проданых товаров с столбцаси: ItemID, ProductID, price_real, shipping_real
  sold_ProductID_price = merge(subset(sold, select = c(ItemID)),
                               subset(publish, select = c(ItemID, ProductID, price_real, shipping_real)),
                               by.x = "ItemID",
                               by.y = "ItemID");
  #таблица с столбцами: ebaycategory_id, count_sold
  sold_table = data.frame(table(merge(subset(sold_ProductID_price, select = c(ProductID)),  
                                      subset(products, select = c(ProductID, ebaycategory_id)),
                                      by.x = "ProductID", 
                                      by.y = "ProductID"
  )$ebaycategory_id));
  names(sold_table) = c("ebaycategory_id", "count_sold");
  
  
  
  
  #таблица с столбцами: ebaycategory_id, count_push
  push_table = data.frame(table(merge(subset(publish, select = c(ProductID)),
                                      subset(products, select = c(ProductID, ebaycategory_id)),
                                      by.x = "ProductID", 
                                      by.y = "ProductID")$ebaycategory_id));
  names(push_table) = c("ebaycategory_id", "count_push");
  
  #таблица с столбцами: ebaycategory_id, count_sold, count_push
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
  }#добавл€ем в таблицу частот среднюю цену проданых товаров в даной категории(mean_price)
  
  #добавл€ем веро€тность продажи(prob)
  table_category = transform(table_category, prob = pmin(1,count_sold/count_push));
  
  #добавл€ем среднюю прибыль за мес€ц(prof_mounth)
  table_category = transform(table_category, prof_mounth = (prob*mean_price/10-0.05)*count_push/7);
  
  #добавл€ем веро€тность при новой системе выставлени€(new_prob)
  table_category = transform(table_category, new_prob = pmin(1,count_sold*delta_prob/(count_push+(delta_prob-1)*count_sold)));
  
  #добавл€ем новую среднюю прибыль за мес€ц(new_prof_mounth)
  table_category = transform(table_category, new_prof_mounth = (new_prob*mean_price/10-0.15)*(count_push+(delta_prob)*count_sold)/7);
  
  #добавл€ем изменение средней прибыли за мес€ц(delta_prof_mounth)
  table_category = transform(table_category, delta_prof_mounth = new_prof_mounth - prof_mounth);
  
  return(table_category);
}#таблица частот дл€ товаров из разнных категорий


saveTable <- function(table,
                      waySave = myRoute,
                      name,
                      sepSave = "|"){
  
  
  write.table(head(data.frame(table[1],round(table[-1], digits = 3)), countView),
              file = paste0(waySave, name, ".csv"),
              sep = sepSave,
              row.names = FALSE);
  
}#функци€ дл€ сохранени€ таблицы

result = "a";

saveTable(table = tableOfCategory(), name = result);

result;
}