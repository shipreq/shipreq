FROM postgres:11.5

RUN apt-get -qq update \
 && apt-get -qq -y --no-install-recommends install \
      postgresql-11-hll

COPY ./create-extensions.sql /docker-entrypoint-initdb.d/00-create-extensions.sql
