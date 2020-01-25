create or replace view users_by_type as
  select user_type_by_username(username) user_type,
         hll_add_agg(hll_hash_bigint(id)) users,
         count(1) count
  from usr
  where username is not null
  group by user_type_by_username(username);

create or replace view projects_by_owner_type as
  select user_type_by_username(username) owner_type,
         hll_add_agg(hll_hash_bigint(p.id)) projects,
         count(1) count
  from project p, usr u
  where usr_id = u.id
    and username is not null
  group by user_type_by_username(username);
