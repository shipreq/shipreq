function urlPathForGA(url) {
    url = url || window.location;
    url = url + ''; // window.location is fucking mutable and will actually change the url. WTF!
    return url
        .replace(/^.+?\/\//,'')      // Strip protocol
        .replace(/^.+?(?:\/|$)/,'/') // Strip domain
        .replace(/[#?].*$/,'')       // Strip anchor and query
        .replace(/(\/(?:register|resetpw|project|share|usecase)\/)[a-zA-Z0-9]+/,'$1___')
    ;
}

(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-47038103-1', 'shipreq.com');
ga('set', 'page', urlPathForGA());
ga('send', 'pageview');
