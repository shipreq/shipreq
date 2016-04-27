create or replace function to_iso8601_str(timestamp with time zone) returns varchar as $$
  select 'obsolete, commented out due to libxml dependency'::varchar --case when $1 is null then null else substring(xmlelement(name x, $1)::varchar from 4 for 32) end
$$ language sql immutable;

-- select to_iso8601_str(now()), to_iso8601_str(NULL);
