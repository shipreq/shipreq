-- Move the usecase_rev.number column onto usecase

/*
select * from usecase u, usecase_rev r
where r.id = latest_rev_id
order by r.number
*/

ALTER TABLE usecase ADD COLUMN number INT2 NULL CHECK (number > 0);

UPDATE usecase SET number = r.number
FROM usecase_rev r WHERE r.id = latest_rev_id;

ALTER TABLE usecase ALTER COLUMN number SET NOT NULL;

------------------------------------------------------------------------------------------------

drop view if exists v_uc_latest;
drop view if exists v_uc_short;
drop view if exists v_uc_full;

create or replace view v_uc_full as
  select
     ucr.ident_id "uc_id"
    ,ucr.id = u.latest_rev_id "uc_is_latest"
    ,ucr.rev "uc_rev"
    ,ucr.id "uc_rev_id"
    ,u.number "uc_number"
    ,ucr.title "uc_title"
    ,ucr.created_at "uc_created_at"
    ,(select count(1) from uc_field ff where ucr.id = ff.uc_rev_id) "rels"
    ,f.label "r_label"
    ,f.parent_rev_id "r_parent_rev_id"
    ,f.index "r_index"
    ,tr.text "t_text"
    ,tr.ident_id "t_id"
    ,tr.rev "t_rev"
    ,tr.id "t_rev_id"
    ,tr.created_at "t_created_at"
    ,fk.id "fk_id"
    ,fkt.name || (case when fk.data is not null then ': ' || fk.data else '' end) "fk_desc"
  from usecase_rev ucr
    join usecase u on ucr.ident_id = u.id
    left join uc_field f on ucr.id = f.uc_rev_id
    left join text_rev tr on tr.id = f.text_rev_id
    left join text t on tr.ident_id = t.id
    left join field_key fk on fk.id = t.fk_id
    left join field_key_type fkt on fk.type_id = fkt.id
  order by
    u.number, ucr.rev
    , fk.id, f.label, f.index
;

create view v_uc_short as
  select
    uc_rev_id
    ,'UC-' || uc_number || ': ' || uc_title "uc_desc"
    ,uc_rev
    ,fk_desc
    ,r_label
    ,t_text
    ,t_rev_id
    ,t_id
  from v_uc_full
;

create view v_uc_latest as
  select
    uc_rev_id
    ,'UC-' || uc_number || ': ' || uc_title "uc_desc"
    ,fk_desc
    ,r_label
    ,t_text
    ,t_rev_id
    ,t_id
  from v_uc_full
  where uc_is_latest = true
;

------------------------------------------------------------------------------------------------

ALTER TABLE usecase_rev DROP COLUMN number;