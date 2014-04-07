CREATE USER usecase_prod PASSWORD 'ucelocal';
ALTER USER usecase_prod CREATEDB;
CREATE DATABASE usecase_prod OWNER usecase_prod ENCODING 'utf8';

