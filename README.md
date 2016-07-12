﻿# BigData 
 - Загрузка стабильной версии API для работы с R скриптами в репозиторий
 - Загрузка стабильных R скриптов в репозиторий
 - Composer инициализирует поект и вытягивате зависимости 

    (проект готов к работе)

 - Обращение к скрипту 
 - Настройка пользователем параметров для обработки данных
 - Передача параметров R скрипту
 - Получение ответа от R скрипта
 - Вывод результата пользователю

# запуск сервера
- запускаем Rscript server.R передаем ему полный путь config.csv
    - Загрузка config
    - Подключение readData.R в котором считываються все параметры для доступа к базе данных и описаны функции для получения таблиц из базы.
    - Подключение скриптов которые будут возвращать данные для построения графиков
    - Подключение скриптов которые будут возвращать таблицы
    - Старт сервера
        - сервер принимает POST запрос с параметрами (scriptName, ...)
        - обработка запроса 
        - отправка ответа в Json формате

# структура папок
- bigData - rootdir
    - res - resourse(config, ...)
    - src - all scripts


# Документация к сервисным скриптам 
- server.R
	- Скрипт который запускает сервер по обработке запросов
	- Принимает через командную строку адрес config.csv

- readData.R
	- Скрипт в котором описаны функции получения и обработки данных из бызы

- decoder.R
	- Скрипт для парсинга RQL запроса
	- В начале определены константы которые отвечают за название логических и скалярных операторов 

# Документация к скриптам 
- plotPublishPrice.R
	- Имя отчета: гистограмма цены выставленных товаров
	- Принимает: бренд, номер категории, начальная дата, конечная дата 
	- Ось Х: логарифм по основанию 10 от цены товара (float)
	- Ось У: количество выставленных товаров из данного ценового интервала (int)
	- Возвращает: JSON вида {id,x*,y} *x-центры ценовых промежутков
	- начальная дата 2015-06-01

- plotSoldPrice.R
    - Имя отчета: гистограмма цены проданых товаров
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: логарифм по основанию 10 от цены товара (float)
    - Ось У: количество проданных товаров из данного ценового интервала (int)
    - Возвращает: JSON вида {id,x*,y} *x-центры ценовых промежутков
    - начальная дата 2015-06-01

- plotProbPrice.R
    - Имя отчета: график вероятности продажи товара из заданной ценовой категории
    - Принимает: бренд, номер категории, начальную дата, конечная дата
    - Ось Х: логарифм по основанию 10 от цены товара (float)
    - Ось У: Оценка вероятности продажи товара из данной ценовой категории (float)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotProfPrice.R
    - Имя отчета: график прибыли от одного выставления товара из заданой ценовой категории
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: логарифм по основанию 10 от цены товара (float)
    - Ось У: Оценка прибыли от одного выставления товара из заданой ценовой категории (float)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotPublishDay.R
    - Имя отчета: гистограмма количества выставлений по дням недели
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: день недели (int)
    - Ось У: количество товаров которые были выставленый в данный день недели (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotSoldDay.R
    - Имя отчета: гистограмма количества породаж в зависимости от дня выставления
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: день недели (int)
    - Ось У: количество проданых товаров которые были выставлены в данный день недели (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotProbDay.R
    - Имя отчета: график вероятности продажи товара выставленного в заданый день недели
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: день недели (int)
    - Ось У: оценка вероятности продажи товара выставленного в заданный день недели (float)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotCreatedDay.R
    - Имя отчета: гистограмма количества продаж в каждый день недели 
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: день недели (int)
    - Ось У: количество товаров проданых в данный день (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotPublishTime.R
    - Имя отчета: гистограмма количества выставлений в каждый часв течении дня
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: время суток(час) (int)
    - Ось У: количество выставленых в заданый чвс товаров (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotSoldTime.R
    - Имя отчета: гистограмма количества проданных товаров выставленых в заданое время
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: время суток(час) (int)
    - Ось У: количество проданных товаров которые были выставлены в заданое время (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotProbTime.R
    - Имя отчета: график вероятности продажи товара выставленного в заданое время суток
    - Принимает: бредн, намер категории, начальная дата, конечная дата
    - Ось Х: время суток(час) (int)
    - Ось У: Оценка вероятности продажи товара выставленого заданое время суток (float)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotCreatedTime.R
    - Имя отчета: гистограмма количетва продаж в каждый час дня
    - Принимает бренд, номер категории, начальная дата, конечна дата
    - Ось Х: время суток(час) (int)
    - Ось У: количество продынных товаров в заданое время суток (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- plotCreatedTimeWithTZ.R
    - Имя отчета: гистограмма количества продаж в каждый час дня с учетом часовых почсов
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Ось Х: время суток в штате где было куплено товар (int)
    - Ось У: количество продынных товаров в заданое время суток с учетом часовых поясов (int)
    - Возвращает: JSON вида {id,x,y}
    - начальная дата 2015-06-01

- tableCategoryPrice.R
    - Имя отчета: таблица частот по ценовым категориям товаров
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Столбцы:  category_price(ценовая категория)(int), count_sold(количество проданных)(int), count_publish(количество выставленых)(int), prob(оценка вероятности продажи)(float), prof_mounth(оценка прибыли за месяц)(float), new_prob(оценка вероятности продажи при выставлениия на 10 дней)(float), new_prof_mounth(оценка прибыли за месяц при выставлении на 10 дней)(float),delta_prof_mounth(разница в прибыли)(float), id(id строки)(int)
    - Возвращает: JSON таблицу с задаными елементами
    - начальная дата 2015-06-01

- tableCategoryID.R
    - Имя отчета: таблица частот по категориям товаров
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Столбцы: ebaycategory_id(номер категории)(int), count_sold(количество приданных)(int), count_push(количество выставленных)(int), mean_price(средняя цена по категории)(float), prob(оценка вероятности продажи)(float), prof_mounth(оценка прибыли за месяц)(float), new_prob(оценка вероятности продажи при выставлении на 10 дней)(float), new_prof_mounth(оценка прибыли за месяц при выставлении на 10 дней)(float), delta_prof_mounth(разница в прибыли)(float), id(id строки)(int)
    - ВВозвращает: JSON таблицу с задаными елементами
    - начальная дата 2015-06-01

- tableProduct.R
    - Имя отчета: таблица частот по отдельным товарам
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Столбцы: ProductID(int), count_sold(количество проданных)(int), count_push(количество выставленных)(int), price(цена товара)(float), prob(оценка вероятности продажи)(float), prof_mounth(оценка прибыли за месяц)(float), new_prob(оценка вероятности продажи пир выставлении на 10 дней)(float), new_prof_mounth(оценка прибыли за месяц при выставлении на 10 дней)(float), delta_prof_mounth(разница прибыли)(float), id(id строки)(int)
    - Возвращает: JSON таблицу с задаными елементами
    - начальная дата 2015-06-01

- tableModel.R
    - Имя отчета: таблица популярности каждой из марок мотоциклов
    - Столбцы: vehicle_id(id марки мотоцикла)(int), count_sold(количество проданных товаров которые подходят данной марке)(int), count_publish(количество выставленных товаров которые подходят данной марке)(int), id(id строки)(int), vehicles(название марки мотоцикла)(text)
    - Возвращает: JSON таблицу с задаными елементами
    - начальная дата 2015-06-01

- tableProductModel.R
    - Имя отцета: таблица популярности деталей по маркам которым она подходит
    - Столбцы: ProductID(int), count_model_sold(количество проданых товаров которы подходят тем же маркам мотоциклов)(int), count_model_publish(количество выставленных товаров которые подходят тем же маркам мотоцыклов)(int), prob(оценка вероятности продажи товара который подходит тем же маркам мотоциклов)(float), id(id строки)(int)
    - Возвращает: JSON таблицу с задаными елементами
    - начальная дата 2015-06-01

- getBrand.R
    - Имя отчета: список брендов товаров которые есть у нас в наличии
    - Столбцы: id(id строки)(int), name(название бренда)(text), value(значение бренда совпадает с названием)(text)
    - Возвращает: JSON таблицу с задаными елементами

- getCategory.R
    - Имя отчета: список категорий товаров которые есть у нас в наличии 
    - Столбцы: id(id строки)(int), name(имя категоии)(text), value(номер категории)(int)
    - Возвращает: JSON таблицу с задаными елементами

- bestProducts.R
    - Имя отчета: список товаров которые продались больше всего на ebay
    - Столбцы: id(id строки)(int), title(тайтл товара)(text), count_sold(количество продаж данного товара начиная с 2016-01-22)(int)
    - Возвращает JSON таблицу с задаными елементами
- bestCompetitor.R
    - Имя отчета: список продавцов и количество продаж этих продавцов
    - Столбцы: id(id строки)(int), seller_name(ник продавца)(text), count_sold(количество продаж данного продавца с 2016-01-22)(int)
    - Возвращает: JSON таблицу с задаными елементами
- NN.R
    - Имя отчета: гистограмма цен выставленных товаров с задаными словами
    - Принимает: начальная дата(start_time>=), конечная дата(start_time<=), слова которые должны встречаться в title(title like), слова которые не должны встречаться в title(title not like)
    - Ось Х: логарифм по основанию 10 от цены товара (float)
    - Ось у: количество выставлений (int)
    - Возвращает: JSON{id, x, y}

- NNSold.R
    - Имя отчета: гистограмма цен проданых товаров с задаными словами
    - Принимает: начальная дата(start_time>=), конечная дата(start_time<=), слова которые должны встречаться в title(title like), слова которые не должны встречаться в title(title not like)
    - Ось Х: логарифм по основанию 10 от цены товара (float)
    - Ось у: количество продаж (int)
    - Возвращает: JSON{id, x, y}
- tablePublishTime.R
    - Имя отчета: таблица количества выставлений на каждый час недели
    - Принимает: ничего
    - Возвращает: JSON матрицы 24*7 с количеством выставлений на каждый час (int)
- soldProduct.R
    - Имя отчета: таблица количества продаж каждого из товаров
    - Принимает: бренд, номер категории, начальная дата, конечная дата
    - Возвращает: JSON таблицы ProductID(int), count_sold(int)