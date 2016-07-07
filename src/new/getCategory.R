# какие столбцы
#   product ebaycategory_id category

getCategoryName<- function(){
  
  # создаем запросы
  {
    queryProduct ="SELECT category.id, category.name, category.parentID FROM dima_db.products, dima_db.category Where products.category_id_path like concat('%', category.id, '%') GROUP BY category.id;"
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
      res = Product
      
      res = data.frame(id = 1:nrow(res), res[c("name","id", "parentID")]);
      
      names(res) = c("id", "name", "value", "parentID");
      
      return(res)
    }
    
    return(getCategory())
  }
}