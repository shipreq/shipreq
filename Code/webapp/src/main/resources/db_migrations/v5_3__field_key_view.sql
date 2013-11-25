CREATE VIEW v_field_key AS
select
  f.id fk_id
  ,t.id type_id
  , case when data is null
      then t.name
      else t.name || ': ' || data::varchar(100)
    end::varchar "desc"
from field_key f
inner join field_key_type t on t.id = f.type_id
order by 3;

