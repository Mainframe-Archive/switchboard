---
layout: guide
---

## Installation

### Install Dependencies

Switchboard uses [Vagrant](vagrantup.com) for cross-platform local
setup, and [Ansible](started with Vagrant) for deployment. Vagrant
requires [Virtualbox](https://www.virtualbox.org/) to handle its
virtual machines.

So, before setting up Switchboard you must install:

- [Virtualbox](https://www.virtualbox.org/wiki/Downloads) - tested
  on 4.3.15. Once installed, Virtualbox will ask if you want to set
  up a virtual machine. You can skip this; Vagrant will handle it.
- [Vagrant](https://docs.vagrantup.com/v2/installation/index.html) -
  tested on 1.6.3
- [Ansible](http://docs.ansible.com/intro_installation.html) -
  tested on 1.6.1

### Setup Switchboard

Run the following at your shell to get a virtual machine up and
running Switchboard. Once running, the virtual machine is assigned
the IP address `192.168.50.2`.

{% highlight bash %}
git clone https://github.com/thusfresh/switchboard switchboard
cd switchboard
# Create a virtual machine, compile Switchboard, and start it up
vagrant up

## Using Switchboard from within the virtual machine:
# Login to the virtual machine
vagrant ssh
# To see the Switchboard service's options
service switchboard
# To connect a remote shell to the switchboard process
/home/vagrant/switchboard/_rel/switchboard/bin/switchboard remote_console
{% endhighlight %}


### Troubleshooting

Ansible sometimes
[times out](https://github.com/ansible/ansible/issues/6751) when
fetching the Erlang .deb file. If this happens, please check your
network connection and retry.

Please create an issue if there were any problems getting setup --
we'd love to get you up and running.

### Provisioning

Switchboard's Ansible provisioning scripts can be used to deploy the
application on remote servers. To provision a server it must be
listed in an inventory file.

There is an example inventory file under `provisioning/hosts.example`.
To customize, list the servers under the `[switchboard]` heading,
and set custom variables under `[switchboard:vars]`. See
`provisioning/roles/switchboard/default/main.yml` for a list
of variables which can be customized.

Coming soon: Description of deploying from your own switchboard repo.

If your inventory is located at `provisioning/hosts`, you can
provision switchboard by running:

{% highlight bash %}
ansible-playbook -i provisioning/hosts provisioning/playbook.yml
{% endhighlight %}

Next up:
[worker and client interfaces]({{site.baseurl}}/interfaces).
