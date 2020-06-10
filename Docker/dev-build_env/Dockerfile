FROM archlinux/base

RUN pacman -Sy --noconfirm --needed archlinux-keyring

RUN pacman -Sy --noconfirm reflector \
 && reflector --country Australia --country Canada --latest 100 --sort rate --save /etc/pacman.d/mirrorlist

RUN pacman -Syyu --noconfirm

RUN echo 'en_AU.UTF-8 UTF-8' >> /etc/locale.gen \
 && locale-gen \
 && echo 'LANG=en_AU.UTF-8' > /etc/locale.conf

ENV LANGUAGE=en_AU:en \
    LANG=en_AU.UTF-8 \
    LC_ALL=en_AU.UTF-8

# Modify makepkg to allow running as root
RUN sed -i -e '/as root is not allowed/,+2 {s/^/japgolly= #/}' /usr/sbin/makepkg

##############
# Most stuff #
##############

RUN pacman -Sy --noconfirm --needed \
      awk \
      binutils \
      brotli \
      diffutils \
      docker \
      docker-compose \
      fakeroot \
      file \
      git \
      htop \
      parallel \
      pigz \
      procps \
      sudo \
      tree \
      unzip \
      which \
      zip

#########
# Graal #
#########

RUN n=jdk11-graalvm-bin \
 && cd /tmp \
 && git clone https://aur.archlinux.org/$n.git \
 && cd $n \
 && git checkout 38587e749aa06e8a29d8a21febde89675cd46c34 2>/dev/null \
 && makepkg -sri --noconfirm \
 && cd /tmp \
 && rm -rf $n

########
# Node #
########

ENV NODE_PATH=/root/.npm/lib/node_modules

RUN pacman -Sy --noconfirm --needed npm nodejs \
 && npm config set prefix /root/.npm \
 && npm install -g jsdom

##############
# Phantom JS #
##############

RUN pacman -Sy --noconfirm --needed fontconfig \
 && export PHANTOM_JS="phantomjs-2.1.1-linux-x86_64" \
 && curl -LO https://github.com/Medium/phantomjs/releases/download/v2.1.1/$PHANTOM_JS.tar.bz2 \
 && tar xvjf $PHANTOM_JS.tar.bz2 \
 && rm $PHANTOM_JS.tar.bz2 \
 && mv $PHANTOM_JS /usr/local/share \
 && ln -s /usr/local/share/$PHANTOM_JS/bin/phantomjs /usr/bin

##################
# SBT & Scala.JS #
##################

RUN cd /usr/local/bin \
 && curl -Ls https://git.io/sbt > sbt \
 && chmod +x sbt

RUN mkdir /tmp/scala \
 && cd /tmp/scala \
 && mkdir project \
 && echo 'sbt.version=1.3.12' > project/build.properties \
 && echo 'scalaVersion := "2.13.2"' > build.sbt \
 && echo 'object M{}' > M.scala \
 && sbt test \
 && cd / \
 && rm -rf /tmp/scala

#############
# Local env #
#############

# Needed for ECR login
RUN pacman -Syu --noconfirm --needed aws-cli

COPY strip-ansi /bin
COPY .bashrc /root/.bashrc

RUN chmod -R a=u /root \
 && ln -s /root/.bashrc /.bashrc \
 && ln -s /root/.npmrc /.npmrc