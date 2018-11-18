# Most of this copied from
# - https://github.com/frol/docker-alpine-glibc/blob/master/Dockerfile
# - https://github.com/anapsix/docker-alpine-java/blob/master/8/192b12/jdk/unlimited/Dockerfile
# - https://github.com/oracle/docker-images/blob/master/GraalVM/CE/1.0.0-rc9/Dockerfile

FROM alpine:3.8

ARG GRAAL_VERSION

ENV LANG=C.UTF-8

COPY .profile /root/.profile

RUN apk --no-cache upgrade

RUN apk --no-cache add \
    ca-certificates \
    curl \
    htop \
    procps \
    tree \
    wget \
    vim

# Install glibc
#
# Taken from docker-alpine-glibc with the following changes:
# - don't use apk at all because...
#    1) wget already upgraded
#    2) ca-certificates installed above
#    3) I want to keep both around
RUN ALPINE_GLIBC_BASE_URL="https://github.com/sgerrand/alpine-pkg-glibc/releases/download" && \
    ALPINE_GLIBC_PACKAGE_VERSION="2.28-r0" && \
    ALPINE_GLIBC_BASE_PACKAGE_FILENAME="glibc-$ALPINE_GLIBC_PACKAGE_VERSION.apk" && \
    ALPINE_GLIBC_BIN_PACKAGE_FILENAME="glibc-bin-$ALPINE_GLIBC_PACKAGE_VERSION.apk" && \
    ALPINE_GLIBC_I18N_PACKAGE_FILENAME="glibc-i18n-$ALPINE_GLIBC_PACKAGE_VERSION.apk" && \
    echo \
        "-----BEGIN PUBLIC KEY-----\
        MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEApZ2u1KJKUu/fW4A25y9m\
        y70AGEa/J3Wi5ibNVGNn1gT1r0VfgeWd0pUybS4UmcHdiNzxJPgoWQhV2SSW1JYu\
        tOqKZF5QSN6X937PTUpNBjUvLtTQ1ve1fp39uf/lEXPpFpOPL88LKnDBgbh7wkCp\
        m2KzLVGChf83MS0ShL6G9EQIAUxLm99VpgRjwqTQ/KfzGtpke1wqws4au0Ab4qPY\
        KXvMLSPLUp7cfulWvhmZSegr5AdhNw5KNizPqCJT8ZrGvgHypXyiFvvAH5YRtSsc\
        Zvo9GI2e2MaZyo9/lvb+LbLEJZKEQckqRj4P26gmASrZEPStwc+yqy1ShHLA0j6m\
        1QIDAQAB\
        -----END PUBLIC KEY-----" | sed 's/   */\n/g' > "/etc/apk/keys/sgerrand.rsa.pub" && \
    wget --retry-connrefused \
        "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" \
        "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" \
        "$ALPINE_GLIBC_BASE_URL/$ALPINE_GLIBC_PACKAGE_VERSION/$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" && \
    apk add --no-cache \
        "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" \
        "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" \
        "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME" && \
    rm "/etc/apk/keys/sgerrand.rsa.pub" && \
    /usr/glibc-compat/bin/localedef --force --inputfile POSIX --charmap UTF-8 "$LANG" || true && \
    echo "export LANG=$LANG" > /etc/profile.d/locale.sh && \
    apk del --no-cache glibc-i18n && \
    rm "/root/.wget-hsts" && \
    rm \
        "$ALPINE_GLIBC_BASE_PACKAGE_FILENAME" \
        "$ALPINE_GLIBC_BIN_PACKAGE_FILENAME" \
        "$ALPINE_GLIBC_I18N_PACKAGE_FILENAME"


# Change to /opt/graal later
ADD graalvm-ce-${GRAAL_VERSION}-linux-amd64.tar.gz /opt/

# Most deletions taken from docker-alpine-java
RUN cd /opt \
 && ln -s graalvm-ce-${GRAAL_VERSION} graal \
 && cd graal \
 && rm -rf *src.zip \
           lib/missioncontrol \
           lib/visualvm \
           lib/*javafx* \
           jre/plugin \
           jre/bin/javaws \
           jre/bin/jjs \
           jre/bin/orbd \
           jre/bin/pack200 \
           jre/bin/policytool \
           jre/bin/rmid \
           jre/bin/rmiregistry \
           jre/bin/servertool \
           jre/bin/tnameserv \
           jre/bin/unpack200 \
           jre/lib/javaws.jar \
           jre/lib/deploy* \
           jre/lib/desktop \
           jre/lib/*javafx* \
           jre/lib/*jfx* \
           jre/lib/amd64/libdecora_sse.so \
           jre/lib/amd64/libprism_*.so \
           jre/lib/amd64/libfxplugins.so \
           jre/lib/amd64/libglass.so \
           jre/lib/amd64/libgstreamer-lite.so \
           jre/lib/amd64/libjavafx*.so \
           jre/lib/amd64/libjfx*.so \
           jre/lib/ext/jfxrt.jar \
           jre/lib/ext/nashorn.jar \
           jre/lib/oblique-fonts \
           jre/lib/plugin.jar \
           sample \
           jre/languages/llvm

RUN sed -i 's/#networkaddress.cache.ttl=-1/networkaddress.cache.ttl=10/' jre/lib/security/java.security \
 && echo 'export PATH="${PATH}:/opt/graal/bin"' > /etc/profile.d/path-graal.sh

ENV JAVA_HOME=/opt/graal
