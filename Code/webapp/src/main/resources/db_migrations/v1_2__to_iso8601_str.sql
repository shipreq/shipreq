create function to_iso8601_str(timestamp with time zone) returns varchar as $$
  select substring(xmlelement(name x, $1)::varchar from 4 for 32)
$$ language sql immutable;

