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

ENV GRAAL_VERSION=19.3.0

RUN cd /opt \
 && wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-19.3.0/graalvm-ce-java11-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && tar xzf graalvm-ce-java11-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && rm graalvm-ce-java11-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && ln -s graalvm-ce-java11-${GRAAL_VERSION} graal \
 && cd graal \
 && rm -rf \
      {bin,languages/js/bin}/js \
      {bin,languages/js/bin}/node \
      {bin,languages/js/bin}/npm \
      {bin,lib}/polyglot \
      bin/gu \
      languages/js/npm/doc \
      languages/llvm \
      lib/installer \
      lib/src.zip \
      lib/visualvm \
      tools/chromeinspector

RUN cd /opt/graal \
 && sed -i 's/#networkaddress.cache.ttl=-1/networkaddress.cache.ttl=30/' conf/security/java.security

ENV JAVA_HOME=/opt/graal \
    PATH=${PATH}:/opt/graal/bin

#=============#
# Environment #
#=============#

COPY .bashrc /root/.bashrc

COPY import-files /usr/bin/import-files
