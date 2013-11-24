drop view if exists v_data;
create view v_data as
select

  dt.name "data_type"

  ,case
   when dt.name = 'FieldValue' and fk.data is not null then fkt.name || ': ' || fk.data
   else fkt.name
   end "field_type"

  ,v.data_id

  ,v.rev

  ,v.id "value_id"

  ,case
   when dt.name = 'UseCase'  then uc.number || '. ' || uc.title
   when dt.name = 'FieldKey' then fk.data
   else coalesce(s.text, fv.text)
   end "value_data"

  ,(select count(1) from relation r where r.from_id=v.id) "relations"

  ,v.updated_at

from value v
left join data d             on v.data_id = d.id
left join data_type dt       on d.type_id = dt.id
left join field_value fv     on v.id = fv.id
left join field_key fk       on (fv.field_key_id = fk.id OR v.id = fk.id)
left join field_key_type fkt on fk.type_id = fkt.id
left join step s             on v.id = s.id
left join usecase uc         on v.id = uc.id
order by
  case dt.name
  when 'UseCase'    then '0'
  when 'FieldList'  then '1'
  when 'FieldKey'   then '2'
  when 'FieldValue' then '3'
  else dt.name
  end
  , field_type, data_id, rev;

