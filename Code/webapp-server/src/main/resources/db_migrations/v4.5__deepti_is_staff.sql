create or replace function user_type_by_username(varchar) returns varchar as $$
  select case
           when $1 = 'japgolly'
             or $1 = 'deepti'
                then 'staff'
           else
             'public'
         end
$$ language sql immutable;
