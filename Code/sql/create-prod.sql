CREATE USER shipreq_prod PASSWORD 'ucelocal';
ALTER USER shipreq_prod CREATEDB;
CREATE DATABASE shipreq_prod OWNER shipreq_prod ENCODING 'utf8';

