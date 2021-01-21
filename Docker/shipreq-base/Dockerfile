FROM amazonlinux:2.0.20201111.0

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

# Note: when changing this, make sure to also change:
#   - :/Code/project/Dependencies.scala
#   - :/Docker/dev-build_env/Dockerfile (the aur/jdk11-graalvm-bin git sha)
ENV GRAAL_VERSION=20.3.0

RUN cd /opt \
 && f=graalvm-ce-java11-linux-amd64-${GRAAL_VERSION}.tar.gz \
 && wget https://github.com/graalvm/graalvm-ce-builds/releases/download/vm-${GRAAL_VERSION}/$f \
 && tar xzf $f \
 && rm $f \
 && ln -s graalvm-ce-java11-${GRAAL_VERSION} graal \
 && cd graal \
 && rm -rf \
           {bin,languages/js/bin}/{js,node,npm} \
           bin/{gu,polyglot} \
           languages/llvm \
           lib/installer \
           lib/visualvm \
           tools

RUN cd /opt/graal \
 && sed -i 's/#networkaddress.cache.ttl=-1/networkaddress.cache.ttl=10/' conf/security/java.security

ENV JAVA_HOME=/opt/graal \
    PATH=${PATH}:/opt/graal/bin

#=============#
# Environment #
#=============#

COPY .bashrc /root/.bashrc

COPY import-files /usr/bin/import-files
