---
layout: guide
---

## Installation

### Install Dependencies

Switchboard uses [Vagrant](vagrantup.com) for cross-platform local
setup, and [Ansible](started with Vagrant) for deployment.

Before getting started you must install:

- [Virtualbox](https://www.virtualbox.org/wiki/Downloads)
- [Vagrant](https://docs.vagrantup.com/v2/installation/index.html)
- [Ansible](http://docs.ansible.com/intro_installation.html)

### Setup Switchboard

Run the following at your shell to get a virtual machine up and
running Switchboard. The virtual machine can be accessed at
`192.168.50.2`.

{% highlight bash %}
git clone https://github.com/thusfresh/switchboard switchboard
cd switchboard
# Create a virtual machine, compile Switchboard, and start it up
vagrant up
# Login to the virtual machine
vagrant ssh

## Using Switchboard from within the virtual machine:
# To see the Switchboard service's options
service switchboard
# To connect a remote shell to the switchboard process
/home/vagrant/switchboard/_rel/switchboard/bin/switchboard remote_console
{% endhighlight %}

Please create an issue if there were any problems getting setup.

Next up:
[worker and client interfaces]({{site.baseurl}}/interfaces).
