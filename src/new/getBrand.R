# какие столбцы
#   product brand

tableBrand<- function(){
  
  # создаем запросы
  {
    queryProduct =paste("select analit_products_by_brand_category.brand ",
                        "from ", myDbname, ".analit_products_by_brand_category;", sep = "");
  }
  
  # считываем таблицы
  {
    product = readTable(queryProduct);
  }
  
  if(!checkTable(product))
  {
    return("ERROR");
  }#  если в таблице не достаточно элементов тогда пишем ERROR
  
  # если достаточно элементов тогда выполняем скрипт дальше
  if(checkTable(product))
  {
    # функция обработки таблицы
    {
      product = change.products(product)
    }
    
    getBrand <- function(Product = product)
    {
      res = data.frame(table(Product$brand))["Var1"]
      
      res = data.frame(id = 1:nrow(res), res, res);
      
      names(res) = c("id", "name", "value");
      
      return(res)
    }
    
    return(getBrand())
  }
}