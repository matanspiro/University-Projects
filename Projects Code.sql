select top 10 con.ID_Product, con.ID_Order, con.Quantity
from Orders as o join Containments as con on o.ID_Order = Con.ID_Order join
Products AS P ON con.ID_Product = p.ID_Product
where year(o.DT_Order) > 2018 
group by con.ID_Product, con.Quantity, con.ID_Order
having con.Quantity>=avg(con.Quantity)
order by 3 desc

-----------------------------------------------------

select p.ID_Product, rev.Stars
from Registered AS R JOIN Reviews AS REV ON R.Email = REV.Email  JOIN Products AS P ON   
     REV.ID_Product = P.ID_Product
where YEAR(REV.DT_Review)> 2018  AND R.Gender = 'M' and rev.Stars<=3
ORDER BY 2 asc

-----------------------------------------

select p.ID_Product, p.Price, o.Discount, avgDiscount= (select avg (Discount) from Orders)
from Orders as o join Shipping_Times as s on o.ID_Order=s.ID_Order 
	join Products as p on p.ID_Product=s.ID_Product
where p.Category_Product='SALE' and o.Discount>=10 and year(DT_Shipping)>2015
group by p.ID_Product, p.Price,  o.Discount
having p.Price<(select avg (price) from Products)
order by 2 desc

-----------------------------------------------------
select r.Email,s.ID_Product
from Registered as r join saved as s on r.Email=s.Email
where r.Email not in (select s.Email from Shipping_Times as sh join orders as o on sh.ID_Order=o.ID_Order
						 where sh.ID_Order=s.ID_Product and o.Email=s.Email)
order by 2

-----------------------------------------

update  PRODUCTS 
set Price = PRODUCTS.Price * 0.95
where PRODUCTS.Price > (select  AVG(Price) from PRODUCTS where Category_Product = 'FOR KIDS')
		and  Category_Product = 'FOR KIDS'

select price, ID_Product
from Products
where Category_Product = 'FOR KIDS' and (ID_Product=2 or ID_Product= 3496 or ID_Product=4970)
order by 1 desc

---------------------------------------------

select r.ID_Product
from Shipping_Times as st join Reviews as r on st.ID_Product=r.ID_Product
group by stars, r.ID_Product
having Stars<(select avg (cast(Stars as real)) from Reviews)

intersect

select p.ID_Product
from Products as p join findings as f on p.ID_Product=f.ID_Product
where month(f.DT_Search)=11 and Price>(select avg (cast(price as real)) from Products)

--------------------------------------------------------

CREATE VIEW v_passValidation AS 
SELECT r.Email
FROM Registered as r
WHERE LEN(r.Pass) <=8
		 OR (r.pass NOT LIKE '%[0-9]%' 
			AND  r.pass NOT LIKE '%[A-Za-z]%')


SELECT * FROM v_passValidation

---------------------------------------------------------

CREATE FUNCTION f_findTransaction (@transID int)
returns real as
begin
	declare @price real

		SELECT @price=o.Shipping_Cost+cast((c.Quantity)*(p.Price)*(1-o.Discount/100) as real)
		FROM  products as p join Containments AS c on c.ID_Product=p.ID_Product 
			 join orders as o on c.ID_Order=o.ID_Order
			 join Credit_Cards as ccards on o.CC#=ccards.CC#
		WHERE o.ID_Order=@transID 

	return @price
end


select price=dbo.f_findTransaction(30)

------------------------------------------------------

CREATE 	FUNCTION f_highestRatedProducts ()
RETURNS TABLE
AS RETURN
select top 5 ID_Product,bestRank=avg(Cast(Stars as real))
from Reviews
where year(getdate())=year(DT_Review) and MONTH(getdate())-MONTH(DT_Review)<=2
group by ID_Product,Stars
order by Stars desc


select *from dbo.f_highestRatedProducts()

---------------------------------------------------------
alter table registered add numOfOrders int

update Registered
set numOfOrders=1


create trigger t_updateNumOfOrders
on orders 
after insert
as

update Registered
set Registered.numOfOrders=Registered.numOfOrders+
							(select count(*) from inserted where Registered.Email=inserted.Email)

-----------------------------------------------------

CREATE PROCEDURE sp_specificDiscount @email varchar(50), @additionalDiscount  int
AS
UPDATE ORDERS SET orders.Discount=orders.Discount+@additionalDiscount
WHERE orders.Email=@email


EXECUTE SP_UpdateOrderDiscount 'uhurriondj@phpbb.com', 40

----------------------------------------------------

create view v_OrdersByCountries as
select Country, numOfOrders=count(*)
from Shipping_Times 
group by country


create view v_CountOfCategoriesByOrders as
select p.Category_Product, summation=count (*)
from Orders as o join Shipping_Times as st on o.ID_Order=st.ID_Order 
		join products as p on st.ID_Product=p.ID_Product 
group by p.Category_Product


create view v_starsPerCategory as
select p.Category_Product, avgStars=avg(Cast(r.Stars as decimal(3,2)))
from Reviews as r join Products as p on r.ID_Product=p.ID_Product
group by p.Category_Product


create view v_salesPerYear
as
select distinct sos.yearOfOrders ,sumOfSales=sum(sos.sumOfSalesPerOrder)
from (
		select distinct yearOfOrders=year(o.DT_Order), sumOfSalesPerOrder=p.Price*c.Quantity
		from Products as p join Containments as c on p.ID_Product=c.ID_Product   
			join orders as o on c.ID_Order=o.ID_Order
		) as sos
group by sos.yearOfOrders

------

create view v_avgFreightPerState as
select st.Country, avgFreight=avg(Cast(o.Shipping_Cost as real))
from Shipping_Times as st join orders as o on st.ID_Order=o.ID_Order
group by st.Country


create view v_avgPriceByCategory as
select Category_Product, avgPrice=avg(Price)
from Products
group by Category_Product


create view v_searchesPerCategory as
select p.Category_Product, sumOfSearches=count (*)
from searches as s join Products as p on s.Category_Search=p.Category_Product
group by p.Category_Product

------------------------------------------------

create function f_totalPerCity ()
returns table
as
return 
	select stt.Country, stt.City, total=count(*)
	from Shipping_Times as stt join orders as o on stt.ID_Order=o.ID_Order
	group by stt.Country, stt.City


create function f_getTopCities ()
returns table
as

return 
	select distinct tt.Country, tt.City, tt.total		
	from f_totalPerCity() as tt
	where tt.total>=all(select dbo.f_totalPerCity.total from f_totalPerCity() where dbo.f_totalPerCity.country=tt.country)
	group by tt.Country, tt.City, tt.total


create function f_getSumPerCountry ()
returns table
as

return 
	select country, summationPerCountry=count(*)
	from Shipping_Times
	group by country


create function f_joinTopCityWithSumPerCountry()
returns table
as
return
select f1.Country,f1.City,f1.total,f2.summationPerCountry,
	 ratio=cast(cast(f1.total as float)/cast(f2.summationPerCountry as float) as float)
from dbo.f_getTopCities() as f1 join f_getSumPerCountry() as f2 on f1.Country=f2.country
group by f1.Country,f1.City,f1.total,f2.summationPerCountry


create procedure creatingTable
as

create table tempTable (
	country varchar(50),
	city varchar(50),
	total int,
	summationPerCountry int,
	ratio float
)

insert into tempTable
select *
from f_joinTopCityWithSumPerCountry()

alter table tempTable add possibleLocation varchar(3)


execute creatingTable


create procedure sp_getTopCityPerCountry
as

declare c1 cursor for
(
	select Country, ratio, possibleLocation
	from tempTable
)
declare 
	@country varchar(20),
	@ratio float,
	@possibleLocation varchar(3)

open c1
fetch next from c1 into @country,@ratio,@possibleLocation
while @@FETCH_STATUS=0
begin
	if @ratio>0.5
		update tempTable
		set possibleLocation='Yes'
		where country=@country
	else
		update tempTable
		set possibleLocation='No'
		where country=@country
	fetch next from c1 into @country,@ratio,@possibleLocation
end
close c1
deallocate c1

select*
from tempTable


execute sp_getTopCityPerCountry

drop table tempTable

---------------------------------------------------------

create view v_getWinnersDeatils
as
SELECT TOP 5 r.email,o.ID_Order,p.ID_Product, p.price, o.Discount, c.Quantity, o.Shipping_Cost,totalCost=price*Quantity*(1-discount/100)
FROM Registered as r join orders as o on r.Email=o.Email join containments c on o.ID_Order=c.ID_Order
	 join products as p on c.ID_Product=p.ID_Product
ORDER BY NEWID()


create function f_getAvgOfView()
returns smallmoney
as begin
declare @avg smallmoney
select @avg=avg(totalCost)
from v_getWinnersDeatils
group by totalCost
return @avg
end


create procedure sp_updateDetailsForWinners
as 
	declare c2 cursor for
	select v.Email, v.Quantity, v.Shipping_Cost,v.totalCost
	from v_getWinnersDeatils as v

	declare @email varchar(50),@quantity int,@shipping_cost smallmoney, @totalCost smallmoney
	open c2 fetch next from c2 into @email,@quantity,@shipping_cost,@totalCost
	while @@FETCH_STATUS=0
	begin

	update orders 
	set Shipping_Cost=0
	where Shipping_Cost=@shipping_cost

	if @totalCost>dbo.f_getAvgOfView()
		update Containments
		set quantity=quantity+1
		where Quantity=@quantity 	

	fetch next from c2 into @email,@quantity,@shipping_cost,@totalcost
	end
	close c2
	deallocate c2


execute sp_updateDetailsForWinners

---------------------------------------------------------

CREATE Function infoOnCategories()
returns table
as return
select a1.Category_Product,a2.ExpressingInterest,a1.numberOfOrders,a1.totalIncom,a1.avgPrice,
		a3.avgStars
from (select p.Category_Product,numberOfOrders=count(*),totalIncom=sum(p.Price*c.Quantity),
		avgPrice= sum(p.Price*c.Quantity)/count(*)
		from Orders as o join Containments as c on o.ID_Order=c.ID_Order
		join Products as p on c.ID_Product=p.ID_Product 
		where  YEAR(dt_order)=YEAR(getdate())
		group by p.Category_Product) as a1
		 left join (select p.Category_Product,ExpressingInterest=count(*)
				from Findings as f  join Products as p on f.ID_Product=p.ID_Product join 
				Searches as s on s.IP_Search=f.IP_Search
				where YEAR(s.DT_Search)=YEAR(getdate())
				group by p.Category_Product) as a2 on a1.Category_Product=a2.Category_Product
				 left join (select p.Category_Product,avgStars=cast((cast(sum(r.Stars)as real)/cast(count(*)as real))as real)
							from Reviews as r join Products as p on p.ID_Product=r.ID_Product
							where YEAR(r.DT_Review)=YEAR(getdate())
							group by p.Category_Product) as a3 on a1.Category_Product=a3.Category_Product


CREATE Function yearsAndIncomes()
returns table
as return
select theYear=YEAR(o.dt_order),total=sum(p.Price*c.Quantity)
from Orders as o join Containments as c on o.ID_Order=c.ID_Order
	 join Products as p on c.ID_Product=p.ID_Product 
where YEAR(getdate())- YEAR(o.dt_order)<=5
group by YEAR(o.dt_order)



CREATE Function infoBestYear()
returns table
as return
select  p.Category_Product,numberOfOrders_bestYear=count(*),
			totalIncom_bestYear=sum(p.Price*c.Quantity),avgPrice_bestYear= sum(p.Price*c.Quantity)/count(*)
from Products as P join  Containments as c on p.ID_Product=c.ID_Product join Orders as o
		on c.ID_Order=o.ID_Order 
where year(o.dt_order)=(select dbo.yearsAndIncomes.theYear
						from dbo.yearsAndIncomes ()
						where dbo.yearsAndIncomes.total=(select MAX(tempT.total)
													from dbo.yearsAndIncomes()as tempT))
group by  p.Category_Product


create view v_businessReport
as
select dbo1.Category_Product,dbo1.ExpressingInterest,dbo1.numberOfOrders,dbo1.totalIncom,dbo1.avgPrice,dbo1.avgStars,
		dbo2.numberOfOrders_bestYear,dbo2.totalIncom_bestYear,dbo2.avgPrice_bestYear
from dbo.infoOnCategories() as dbo1 left join infoBestYear() as dbo2 on dbo1.Category_Product=dbo2.Category_Product

---------------------------------------------------------------------