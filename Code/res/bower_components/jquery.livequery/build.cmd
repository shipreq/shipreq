mkdir build

java -jar ./tools/yuicompressor-2.4.7.jar -o jquery.livequery.min.js jquery.livequery.js

tools\nuget pack -o build

@pause