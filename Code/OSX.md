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
cd ../Docker/dev-postgres
./build
```

### Without Docker

Start sbt and run:
```
up
taskmanServer/run
```

### With Docker

```sh
cd ../Docker/shipreq-base
./build

cd ../../Code
sbt dockers
bin/dev up -d postgres redis taskman webapp
```
