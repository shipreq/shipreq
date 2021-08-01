# https://hub.docker.com/_/archlinux?tab=tags&page=1&ordering=last_updated
FROM archlinux:base-20210221.0.15908

# https://serverfault.com/questions/1052963/pacman-doesnt-work-in-docker-image
# https://github.com/sickcodes/Docker-OSX/pull/150/files
# TEMP-FIX for pacman issue
RUN patched_glibc=glibc-linux4-2.33-4-x86_64.pkg.tar.zst && \
      curl -LO "https://repo.archlinuxcn.org/x86_64/$patched_glibc" && \
      bsdtar -C / -xvf "$patched_glibc"
# TEMP-FIX for pacman issue

RUN pacman -Syy --noconfirm --needed archlinux-keyring

RUN pacman -Sy --noconfirm reflector rsync \
 && reflector \
      --connection-timeout 2 \
      --download-timeout 2 \
      --country Australia \
      --country Canada \
      --country "United States" \
      --latest 8 \
      --sort rate \
      --save /etc/pacman.d/mirrorlist

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
      java-environment-common \
      java-runtime-common \
      parallel \
      pigz \
      procps \
      sudo \
      tree \
      unzip \
      which \
      zip

RUN sed -i 's/^COMPRESSXZ/#COMPRESSXZ/' /etc/makepkg.conf \
 && echo 'COMPRESSXZ=(xz -c -z - --threads=0)' >> /etc/makepkg.conf

#########
# Graal #
#########

# Note: when changing this, make sure to also change:
#   - :/Code/project/Dependencies.scala
#   - :/Docker/shipreq-base/Dockerfile
RUN n=jdk11-graalvm-bin \
 && gitsha=cafa4a24cde0c7f319f8514c0a7b0efe9ef5dbac \
 && cd /tmp \
 && git clone https://aur.archlinux.org/$n.git \
 && cd $n \
 && git checkout $gitsha 2>/dev/null \
 && makepkg -sri --noconfirm \
 && cd /tmp \
 && rm -rf $n

########
# Node #
########

ENV NODE_PATH=/root/.npm/lib/node_modules

# Workaround https://bugs.archlinux.org/task/63547
RUN cd /etc \
 && cp pacman.conf pacman.conf.tmp \
 && echo -e '[testing]\nInclude = /etc/pacman.d/mirrorlist\n[community-testing]\nInclude = /etc/pacman.d/mirrorlist\n' >> pacman.conf \
 && pacman -Sy --noconfirm --needed libnghttp2 \
 && mv pacman.conf.tmp pacman.conf

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
 && echo 'sbt.version=1.5.5' > project/build.properties \
 && echo 'scalaVersion := "2.13.5"' > build.sbt \
 && echo 'object Build{}' > project/Build.scala \
 && echo 'object M{}' > M.scala \
 && sbt test \
 && cd / \
 && rm -rf /tmp/scala

#############
# Local env #
#############

# Needed for ECR login
RUN pacman -Sy --noconfirm --needed aws-cli

COPY strip-ansi /bin
COPY .bashrc /root/.bashrc

RUN chmod -R a=u /root \
 && ln -s /root/.bashrc /.bashrc \
 && ln -s /root/.npmrc /.npmrc
