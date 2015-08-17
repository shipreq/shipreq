CREATE USER shipreq_prod PASSWORD 'sqlocal';
ALTER USER shipreq_prod CREATEDB;
CREATE DATABASE shipreq_prod OWNER shipreq_prod ENCODING 'utf8';

