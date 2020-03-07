FROM node:11-slim

ENV SHIPREQ_DEV_NODE 11

RUN apt-get -qq update \
 && apt-get -qq -y --no-install-recommends install \
      bsdmainutils \
      dos2unix

RUN curl --compressed -o- -L https://yarnpkg.com/install.sh | bash \
 && mv /root/.yarn /yarn

COPY .bashrc /home/node/.bashrc

ENV PATH="/yarn/bin:$PATH"