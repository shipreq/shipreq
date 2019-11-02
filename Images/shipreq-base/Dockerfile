FROM amazonlinux:2

#========#
# System #
#========#

ENV LANGUAGE=en_AU:en \
    LANG=en_AU.UTF-8 \
    LC_ALL=en_AU.UTF-8

RUN yum update -y \
 && yum install -y \
      awscli \
      htop \
      less \
      procps \
      tar \
      tree \
      unzip \
      vim \
      wget \
      xz \
      zip \
 && yum clean all \
 && rm -rf /var/cache/yum

#=======#
# Graal #
#=======#

ENV GRAAL_VERSION=19.2.1

# Most deletions taken from docker-alpine-java
RUN cd /opt \
 && wget https://github.com/oracle/graal/releases/download/vm-${GRAAL_VERSION}/graalvm-ce-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && tar xzf graalvm-ce-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && rm graalvm-ce-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && ln -s graalvm-ce-${GRAAL_VERSION} graal \
 && cd graal \
 && rm -rf *src.zip \
           {,jre/}bin/polyglot \
           {bin,jre/bin,jre/languages/js/bin}/js \
           {bin,jre/bin,jre/languages/js/bin}/node \
           {bin,jre/bin,jre/languages/js/bin}/npm \
           bin/gu \
           jre/languages/js/npm/doc \
           jre/languages/llvm \
           jre/lib/ext/nashorn.jar \
           jre/tools/chromeinspector \
           lib/installer \
           lib/visualvm \
           sample

RUN cd /opt/graal \
 && sed -i 's/#networkaddress.cache.ttl=-1/networkaddress.cache.ttl=10/' jre/lib/security/java.security

ENV JAVA_HOME=/opt/graal \
    PATH=${PATH}:/opt/graal/bin

#=============#
# Environment #
#=============#

COPY .bashrc /root/.bashrc

COPY import-files /usr/bin/import-files
