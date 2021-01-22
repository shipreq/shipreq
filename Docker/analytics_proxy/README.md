# Setup

```sh
git remote add analytics https://github.com/ZitRos/save-analytics-from-content-blockers.git
git fetch analytics
git checkout a5c5320
git checkout -b fork/analytics
```

# Copying custom changes to fork source

```sh
cd $sq/Docker/analytics_proxy
./copy
cd $sq
git checkout fork/analytics
unzip /tmp/analytics_proxy-Xxxxxxxxxx
# Inspect changes
git add -u
git commit -m 'Integrate my custom changes'
git push && git push github fork/analytics
git checkout -
cd Docker/analytics_proxy
```

# Merging upstream fork source

```sh
cd $sq
git checkout fork/analytics
git fetch analytics
git merge analytics/master
./copy
git checkout -
cd $sq/Docker/analytics_proxy
rm -rf src && unzip -o /tmp/analytics_proxy-Xxxxxxxxxx
# Inspect changes
git add .
git commit -m 'Integrate upstream changes'
git push && git push github fork/analytics
git checkout -
cd Docker/analytics_proxy
```

# Masking URLs

```sh
npm install
npm run mask 'www.googletagmanager.com/gtag/js?id=UA-173267009-2'
make sc_mask # stat counter
```

# Testing 1

1. Run `make test` or `make run`
2. Open up static-test/index.html in a browser
3. In the browser tab, click the AdBlock button, then click Open Logger (the 3rd button under the power button)
4. Reload and look at the network tab, ensure all requests succeed
   If you see `net::ERR_BLOCKED_BY_CLIENT` errors, that means AdBlock is blocking requests.

# Testing 2

1. Run `make build`
2. From SBT: `dockers`
3. Run `:/Code/bin/dev up -d`
4. Install a Chrome plugin to disable CORS:
   https://chrome.google.com/webstore/detail/moesif-origin-cors-change/digfbfaphojjndkpccljibejjbppifbc/related?hl=en-US
5. Open http://localhost:14080/
6. Disable CORS, open Dev Tools, reload
7. Ensure that analytics requests went through successfully
