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
  categoryID = ifelse(args[7]!="NA", args[7], "*");
  minPrice = ifelse(args[8]!="NA", args[8], "0");
  maxPrice = ifelse(args[9]!="NA", args[9], "99999");
  brand = ifelse(args[10]!="NA", args[10], "*");
  
}# —читывание параметров

getData <- function(name,
                    bdName = myDbname,
                    category = CategoryID,
                    myBrand = brand,
                    minP = minPrice,
                    maxP = maxPrice)
{
  if(name=="publish")
  {
    return(paste())
  }
  
  if(name=="sold")
  {
    return(paste())
  }
  
}


{
library(RMySQL)
con <- dbConnect(MySQL(),
                 user = myBDUser,
                 password = myBDPassword,
                 host = myHost,
                 dbname=myDbname);

data.publish <- data.frame(dbGetQuery(conn = con, statement = paste("select ItemID, ProductID, price_real, shipping_real from ", myDbname, ".publish;",sep = "")));
data.sold <-data.frame(dbGetQuery(conn = con, statement = paste("select ItemID from ", myDbname, ".sold;", sep="")));

dbDisconnect(con);

}# считывание с базы данных


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


tableProduct <- function(publish = data.publish, 
                         sold = data.sold,
                         delta_prob = myProb){
  
  #таблица частот с столбцами: ProductID, count_sold
  sold_product = data.frame(table(merge(subset(sold, select = c(ItemID)),
                                        subset(publish, select = c(ItemID, ProductID)),
                                        by.x = "ItemID",
                                        by.y = "ItemID")$ProductID));
  names(sold_product) = c("ProductID", "count_sold");
  
  
  #таблица частот с столбцами: ProductID, count_push
  push_product = data.frame(table(publish$ProductID));
  names(push_product) = c("ProductID", "count_push");
  
  #таблица частот с столбцами: ProductID, count_sold, count_push
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
  }#добавл€ем среднюю цену товара
  
  
  
  #добавл€ем веро€тность продажи(prob)
  table_product = transform(table_product, prob = pmin(1,count_sold/count_push));
  
  #добавл€ем среднюю прибыль за мес€ц(prof_mounth)
  table_product = transform(table_product, prof_mounth = (prob*price/10-0.05)*count_push/7);
  
  #добавл€ем веро€тность при новой системе выставлени€(new_prob)
  table_product = transform(table_product, new_prob = pmin(1, count_sold*delta_prob/(count_push+(1-delta_prob)*count_sold)));
  
  #добавл€ем новую среднюю прибыль за мес€ц(new_prof_mounth)
  table_product = transform(table_product, new_prof_mounth = (new_prob*price/10-0.15)*(count_push+(1-delta_prob)*count_sold)/7);
  
  #добавл€ем изменение средней прибыли за мес€ц(delta_prof_mounth)
  table_product = transform(table_product, delta_prof_mounth = new_prof_mounth - prof_mounth);
  
  #сортируем по убыванию разници прибыли
  table_product = table_product[order(-table_product$delta_prof_mounth),];
  
  return(table_product);
}#таблица частот по отдельным товарам



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

saveTable(table = tableProduct(), name = result);

type = "Table";
type

result;
