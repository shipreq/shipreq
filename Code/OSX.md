# Setup

```sh
brew install --cask graalvm/tap/graalvm-community-jdk17

xattr -r -d com.apple.quarantine /Library/Java/JavaVirtualMachines/graalvm-community-openjdk-17

export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-community-openjdk-17/Contents/Home
export PATH="$JAVA_HOME/bin:$PATH"

gu install js
```

# Running locally for the first time

```sh
# Build local docker containers
cd ../Docker/dev-postgres
./build
# cd ../shipreq-base
# ./build

# cd ../../Code
# sbt taskmanServer/docker
# bin/dev up -d postgres redis taskman
```
