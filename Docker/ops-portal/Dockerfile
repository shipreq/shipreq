FROM nginx:1

RUN apt-get update \
 && apt-get -y install \
      curl \
      unzip \
      vim \
 && mkdir -p ~/.vim/syntax \
 && curl https://raw.githubusercontent.com/nginx/nginx/master/contrib/vim/syntax/nginx.vim -o ~/.vim/syntax/nginx.vim \
 && rm -rf /var/lib/apt/lists/*

COPY src/* /root/

CMD /root/start.sh