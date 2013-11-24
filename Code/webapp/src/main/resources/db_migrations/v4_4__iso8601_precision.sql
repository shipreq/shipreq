create or replace function to_iso8601_str(timestamp with time zone) returns varchar as $$
  select to_char($1 at time zone 'UTC', 'YYYY-MM-DD"T"HH24:MI:SS"Z"')
$$ language sql immutable;
