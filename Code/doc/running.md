# Building and Running Locally (via sbt)

### Setup: system

Install the following:

* Install:
  * SBT
  * Graal with Java 17
    * Install the JS module
  * Docker & docker-compose
  * Node
  * brotli
  * parallel
  * pigz
  * unzip

OSX instructions:

```sh
# Install Graal
brew install --cask graalvm/tap/graalvm-community-jdk17

# Grant it permission to run
xattr -r -d com.apple.quarantine /Library/Java/JavaVirtualMachines/graalvm-community-openjdk-17

# Configure local env
export JAVA_HOME=/Library/Java/JavaVirtualMachines/graalvm-community-openjdk-17/Contents/Home
export PATH="$JAVA_HOME/bin:$PATH"

# Install Graal JS module
gu install js

# Install other apps
brew install brotli node parallel pigz sbt
```

### Setup: docker

```sh
cd Docker/dev-postgres
./build
```

### Setup: node for Scala.js

```sh
cd Code
npm install
```

### Running via sbt

```sh
cd Code
sbt
```

Once you're in the sbt console,
1. Type `up` and hit enter. This will start the webapp. (Type `d` + enter to shut it down again.)
2. Type `taskmanServer/run` to start Taskman.

Both the webapp and taskman have to be run at the same time once to initialise both services,
otherwise each will hang waiting on the other.

After both services have been successfully initialised, you can just start the webapp without running taskman.

### Creating an account

1. Navigate to http://localhost:8080
2. Register your email address
3. Look in the sbt console for logging and find a special url associated with your registration request
4. Navigate to that url
5. Follow the UI to complete your account registration

# Building and Running Locally (via docker)

### Setup

```sh
# Build the base image for the app docker images
cd Docker/shipreq-base
./build

# Create a fake CDN for serving the webapp's static assets
cd ../dev-fake_cdn
make build
```

### Building

This will build docker images for both webapp and taskman:

```sh
cd Code
sbt -DMODE=release dockers
```

### Running

```sh
cd Code
bin/dev up -d taskman webapp
```

Navigate to http://localhost:14080

# Running Locally (via prebuilt docker images)

*(Note: this only works for x64 architecture)*

```sh
docker pull ghcr.io/shipreq/taskman:latest
docker pull ghcr.io/shipreq/webapp:latest
docker tag ghcr.io/shipreq/webapp:latest shipreq/webapp:latest
docker tag ghcr.io/shipreq/taskman:latest shipreq/taskman:latest
cd Code
bin/dev up -d taskman webapp
```
