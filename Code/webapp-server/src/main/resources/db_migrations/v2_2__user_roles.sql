alter table usr add column roles varchar check (roles is null or roles ~ '^[a-z]+(,[a-z]+)*$');

update usr set roles = 'admin' where email = 'japgolly@gmail.com';
update usr set roles = 'test' where id < 0;

