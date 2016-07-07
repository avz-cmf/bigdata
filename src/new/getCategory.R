# какие столбцы
#   product ebaycategory_id category

getCategoryName<- function(){
  
  # создаем запросы
  {
    queryProduct =paste("select ebaycategory_id, products.category ",
                        "from ", myDbname, ".products;", sep = "");
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
    myFun <- function(x)
    {
      return(x[1])
    }
    getCategory <- function(Product = product)
    {
      res = data.frame(aggregate(Product,by = list(Product$category_id_path), myFun))
      
      res = data.frame(id = 1:nrow(res), res[c("category","category_id_path")]);
      
      names(res) = c("id", "name", "value");
      
      return(res)
    }
    
    return(getCategory())
  }
}