create function to_iso8601_str(timestamp with time zone) returns varchar as $$
  select 'obsolete, commented out due to libxml dependency'::varchar --substring(xmlelement(name x, $1)::varchar from 4 for 32)
$$ language sql immutable;

