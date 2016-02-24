# какие столбцы
#   sold:  ItemID
#   publish:  ItemID ProductID price_real shipping_real

# параметры которые нужно передавать
#   пользыватель базы данных myBDUser = "admin";
#   пароль к базе данных myBDPassword = "password";
#   myHost = "RDS Host";
#   myDbname = "MyDB";
#	  route = "/var/www/html/img/";
#   myProb = 1.26
#   countView = 100

# дополнительные параметры

# номер категории
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
  categoryID = ifelse(args[7]!="NA", args[7], "*");
  brand = ifelse(args[8]!="NA", args[8], "*");
  
}# —читывание параметров

getData <- function(name,
                    bdName = myDbname,
                    category = CategoryID,
                    myBrand = brand)
{
  if(name == "publish")
    return(paste("select publish.ItemID, publish.ProductID, publish.price_real, publish.shipping_real ",
                 "from ", bdName, ".publish", ", ", bdName, ".products ",
                 "where publish.ProductID=products.ProductID and products.brand = '", myBrand, "' and products.ebaycategory_id = ", category, ";"  , sep = ""));
  
  if(name == "sold")
    return(paste("select sold.ItemID ",
                 "from ", bdName, ".sold ",
                 "where sold.ItemID in (select publish.ItemID from ",bdName, ".publish, ", bdName, ".products where publish.ProductID = products.ProductID and products.brand = '", myBrand,"' and products.ebaycategory_id = ", category, ");", sep = ""));
  
  
}


{
library(RMySQL)
con <- dbConnect(MySQL(),
                 user = myBDUser,
                 password = myBDPassword,
                 host = myHost,
                 dbname=myDbname);

data.publish <- data.frame(dbGetQuery(conn = con, statement = paste("select ItemID, ProductID, price_real, shipping_real from ", myDbname, ".publish;", sep = "")));
data.sold <-data.frame(dbGetQuery(conn = con, statement = paste("select ItemID from ", myDbname, ".sold;", sep = "")));

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
}# вызов процедур обработки считаных таблиц


tableOfPriceCategory <- function(publish = data.publish, 
                                 sold = data.sold,
                                 delta_prob = myProb){
  
  #таблица частот с столбцами: category_price, count_sold
  sold_table = data.frame(table(transform(merge(subset(sold, select = c(ItemID)),
                                                subset(publish,select = c(ItemID, price_real, shipping_real)),
                                                by.x = "ItemID",
                                                by.y = "ItemID"),
                                          category_price = trunc((price_real+shipping_real)/10))$category_price));
  names(sold_table) = c("category_price", "count_sold");
  
  
  #таблица частот с столбцами: category_price, count_push
  push_table = data.frame(table(transform(subset(publish,select = c(price_real,shipping_real)),
                                          category_price = trunc((price_real+shipping_real)/10))$category_price));
  names(push_table) = c("category_price", "count_push");
  
  
  #таблица частот с столбцами: category_price, count_sold, count_push
  table_category_price = merge(sold_table,
                               push_table,
                               by.y = "category_price",
                               by.x = "category_price");
  
  #добавл€ем веро€тность продажи(prob)
  table_category_price = transform(table_category_price, prob = pmin(1,count_sold/count_push));
  
  #добавл€ем среднюю прибыль за мес€ц(prof_mounth)
  table_category_price = transform(table_category_price, prof_mounth = (prob*as.numeric(category_price)-0.05)*count_push/7);
  
  #добавл€ем веро€тность при новой системе выставлени€(new_prob)
  table_category_price = transform(table_category_price, new_prob = pmin(1,count_sold*delta_prob/(count_push+(1-delta_prob)*count_sold)));
  
  #добавл€ем новую среднюю прибыль за мес€ц(new_prof_mounth)
  table_category_price = transform(table_category_price, new_prof_mounth = (new_prob*as.numeric(category_price)-0.15)*(count_push+(1-delta_prob)*count_sold)/7);
  
  #добавл€ем изменение средней прибыли за мес€ц(delta_prof_mounth)
  table_category_price = transform(table_category_price, delta_prof_mounth = new_prof_mounth - prof_mounth);
  
  return(table_category_price);
  
}#таблица частот по ценовым категори€м


saveTable <- function(table,
                      waySave = myRoute,
                      name,
                      sepSave = "|"){
  
  
  write.table(head(data.frame(table[1],round(table[-1], digits = 3)),countView),
              file = paste0(waySave, name, ".csv"),
              sep = sepSave,
              row.names = FALSE);
  
}#функци€ дл€ сохранени€ таблицы


result = "a";

saveTable(table = tableOfPriceCategory(), name = result);

result;
}