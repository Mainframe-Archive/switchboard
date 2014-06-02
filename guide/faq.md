# Troubleshooting

## Why is Switchboard unable to perform a PLAIN LOGIN for my gmail account?

If you are unable to login to a gmail account using PLAIN
authorization, it is commonly because of Google's fraud
detection. Usually, the login must be from a different geographic
location, a remote server, or a proxy. The error should provide
a URL where you can confirm that you initiated the login attempt.

TODO - include shell

You also won't be able to login using PLAIN authorization if you are
using Google's [2-step
verification](http://www.google.com/landing/2step/) without an
[application-specific password]
(https://support.google.com/accounts/answer/185833?hl=en).
