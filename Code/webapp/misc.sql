select * from data_type;
select * from data;
select * from value;
select * from step;
select * from relation;

-- Inspect values
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

-- Inspect field values
select
  v.data_id
  ,dt.name "data_type"
  ,fkt.name "field_type"
  ,v.rev
  ,v.id "value_id"
  ,fk.data "field_key_data"
  ,coalesce(s.text,fv.text) "field_value_data"
  ,v.updated_at
from value v
left join data d             on v.data_id = d.id
left join data_type dt       on d.type_id = dt.id
left join field_value fv     on v.id = fv.id
left join field_key fk       on (fv.field_key_id = fk.id OR v.id = fk.id)
left join field_key_type fkt on fk.type_id = fkt.id
left join step s             on s.id = v.id
order by data_id, data_type, rev;



--  val fieldValues: Map[FieldKeyRec, FieldValueRec],
--  val stepData: Map[Long, String],
--  val relations: Map[RelationType, Map[Long, List[Long]]]

--case class FieldValueRec(
--  valueId: Long,
--  fieldKeyId: Long,
--  fieldData: FieldValueData
--  ) extends Value[DataType.FieldValue]


insert into data values(666,100)
insert into value values(666,666,1)
insert into relation values(666,200,-1,1012);
insert into relation values(666,200,-1,1013);
insert into relation values(666,200,-1,1019);

explain (format yaml) select id, text from step where id in (select to_id from tmp);


rollback



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

-- Row counts on all tables
SELECT relname, n_tup_ins - n_tup_del as rowcount FROM pg_stat_all_tables where relname not like 'pg%' and relname not like 'sql%' order by 1;
