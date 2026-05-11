FROM node:25-slim

ENV SHIPREQ_DEV_NODE=25

RUN apt-get -qq update \
 && apt-get -qq -y --no-install-recommends install \
      bsdmainutils \
      build-essential \
      ca-certificates \
      curl \
      dos2unix \
      python3

RUN curl --compressed -o- -L https://yarnpkg.com/install.sh | bash \
 && mv /root/.yarn /yarn

ENV PATH="/yarn/bin:$PATH"

COPY .bashrc /root/.bashrc

ENV NODE_OPTIONS=--openssl-legacy-provider
