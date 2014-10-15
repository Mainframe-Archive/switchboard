---
layouts: guide
---

## Deploying

Switchboard includes [Ansible] playbooks and scripts to help you get
it running on a server.

{% highlight init %}
[switchboard]
104.131.101.232 ansible_host=104.131.101.232

[switchboard:vars]
switchboard_clone=true
switchboard_git_version=social
switchboard_git_repo=https://github.com/jtmoulia/switchboard
{% endhighlight %}
