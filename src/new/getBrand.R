# какие столбцы
#   product brand

tableBrand<- function(){
  
  # создаем запросы
  {
    queryProduct =paste("select brand ",
                        "from ", myDbname, ".products;", sep = "");
  }
  
  # считываем таблицы
  {
    product = readTable(queryProduct);
  }
  
  if(!checkTable(product))
  {
    return(NULL);
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
      
      all = data.frame(id = nrow(res)+1, name = 'All Brands', value = '0')
      
      res = rbind(res, all)
      
      return(res)
    }
    
    return(getBrand())
  }
}