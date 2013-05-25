select * from data_type;
select * from data;
select * from value;
select * from relation;

select
  dt.name "data_type"
  ,v.data_id
  ,v.rev
  ,v.id "value_id"
  ,v.updated_at
from value v, data d, data_type dt
where v.data_id = d.id
  and d.type_id = dt.id
order by data_type, data_id, rev;


SELECT fk.type_id, fk.data
FROM field_key fk, relation r
WHERE fk.id = r.to_id
  AND r.from_id = 1000
  AND r.type_id = 200
ORDER BY r.index


insert into data values(55,100)
insert into value values(1,55,1)
insert into value values(2,55,2)
insert into value values(3,55,3)

select * from value where data_id = 55
order by rev desc limit 1
  --and rev = 2

insert into data values(55,100)
insert into data values(66,100)
insert into value values(1,55,1)
insert into value values(2,55,2)

insert into value values(2,55,(select ))

insert into value(data_id,rev)
select 66,coalesce(max(rev)+1,1) from value where data_id=66
returning id, rev