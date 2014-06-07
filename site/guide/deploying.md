---
layout: guide
---

## Deploying Switchboard

### Security

It goes without saying that emails are highly sensitive. A Switchboard
application is only as secure as the server which it is on. Beyond
personal use, great care should be taken to ensure that both
Switchboard and its environment are secure.

When deploying Switchboard, you *must* understand the differences
between the [client and worker](/guide/client) interfaces. Unless
you have a good reason otherwise, the worker interface should
never be publicly exposed.

To have control over what interfaces are exposed, it is recommended
that Switchboard's client port, `8080` by default, is closed to
external connections. Individual interfaces can be exposed be exposed
using an HTTP server'
[rewrite](http://nginx.org/en/docs/http/ngx_http_rewrite_module.html)
[rules](http://httpd.apache.org/docs/current/mod/mod_rewrite.html).

{::comment}
TODO - include an example `nginx.conf`.
TODO - deploy scripts
{:/comment}

