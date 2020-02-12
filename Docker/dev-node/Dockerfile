FROM node:11-slim

ENV SHIPREQ_DEV_NODE 11

RUN apt-get -qq update \
 && apt-get -qq -y --no-install-recommends install \
      dos2unix

COPY .bashrc /home/node/.bashrc
