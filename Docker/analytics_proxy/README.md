# Setup

1. git remote add analytics https://github.com/ZitRos/save-analytics-from-content-blockers.git
2. git fetch analytics
3. git checkout a5c5320
4. git checkout -b fork/analytics

# Copying custom changes to fork source

1. ./copy
2. git checkout fork/analytics
3. unzip /tmp/analytics_proxy-Xxxxxxxxxx
4. Inspect changes
5. git add -u
6. git commit

# Merging upstream fork source

1. cd $sq
2. git checkout fork/analytics
3. git fetch analytics
4. git merge analytics/master
5. ./copy
6. git checkout -
7. cd $sq/Docker/analytics_proxy
8. rm -rf src && unzip -o /tmp/analytics_proxy-Xxxxxxxxxx
9. Inspect changes
10. git add .
11. git commit

# Testing

1. Run `./test` or `./build run`
2. Open up test-static/index.html in a browser
3. In the browser tab, click the AdBlock button, then click Open Logger (the 3rd button under the power button)
4. Reload and look at the network tab, ensure all requests succeed
   If you see `net::ERR_BLOCKED_BY_CLIENT` errors, that means AdBlock is blocking requests.
