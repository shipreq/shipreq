Update Procedure
================

### Install new Jetty

o=9.1.0
n=9.1.1
tar xvzf ~/Downloads/jetty-distribution-$n.v????????.tar.gz
mv jetty-distribution-$n.v???????? jetty-$n
git add jetty-$n
git rm -r jetty-$o
(cd webapp && rm jetty-inst && ln -s ../jetty-$n jetty-inst && git add jetty-inst)
git commit -m "Deps: Jetty $o => $n [1/2]"


### Sanitise new Jetty

cd jetty-$n
rm -r */*{jaas,jsp}[.-]* lib/jsp etc/keystore
perl -pi -e 's!^(?=logs/$)!#!' modules/{logging,requestlog}.mod
git add -A .

