---
title: "Systemd Is Cool"
date: 2022-10-02T22:17:12-04:00
type: post
slug: "systemd-is-cool"
tags:
  - systemd
  - infrastructure
showTableOfContents: true
---

# And everything crashed

To quote the [last post]({{< ref "1-state-of-the-app" >}}):
> Going forward, I'd like to set up some scheduled database dumps as backups (even though the data is not very important and I'm backing it up manually), and **set up a systemd service to start/restart the service automatically if my power ever goes out**.

The first time I left to play go without double checking the server, it was down. The `docker-compose logs` don't show anything crash-like, so it's likely that power was interrupted and the Raspberry Pi restarted silently without restarting the service.

(There are also a lot fun nginx logs showing IP scanning, and a lot of less fun nginx logs showing scripts trying all the default login endpoints, but that's probably nothing the worry about.) 

Time to set up something more sophisticated than `docker-compose up -d`.

# Intro to systemd

[systemd](https://systemd.io/) provides a uniform interface to run and manage background processes ("daemons") in Linux.  It's installed by default in Ubuntu (my dev environment) and Manjaro (my production environment), so extra dependencies added.

systemd does a lot of things, but I only need it to:
1. Start our `docker-compose` service when the server boots
1. Restart the `docker-compose` service if it crashes

The plan is to add a new file `/etc/systemd/system/goban.service` which contains everything systemd needs to know to start/restart/manage the goban server. Fortunately, my configuration is pretty simple, so I basically just cribbed everything from https://www.howtogeek.com/devops/how-to-add-your-own-services-to-systemd-for-easier-management/, (great page).

The only snafu is that the service needs to know where the `docker-compose.yml` file is so it can run `docker-compose`. In this case, I technically know exactly where I cloned the repo so I technically could just hardcode that URL, but that's a bit sketchy and will probably lead to bugs in the future if I ever need to reinstall the service on different hardware in a different location.

My solution was to turn the service file into a template and write an install script that fills out the template. [`goban.service`](https://github.com/go-recordkeeper/goban-deploy/blob/ac4628686abe97464fe195d309e2b814270860c9/goban.service) contains this crucial line:

```service
WorkingDirectory=${WORKING_DIRECTORY}
```

[`install-service.sh`](https://github.com/go-recordkeeper/goban-deploy/blob/ac4628686abe97464fe195d309e2b814270860c9/install-service.sh) looks something like this:

```sh
#!/bin/bash

export WORKING_DIRECTORY=$(pwd)

cat goban.service | envsubst > /tmp/goban.service
sudo cp /tmp/goban.service /etc/systemd/system/goban.service
sudo chmod 644 /etc/systemd/system/goban.service
rm /tmp/goban.service
```

There's a little bit of magic happening there. First we use `pwd` to set `$WORKING_DIRECTORY` to the current directory, which is hopefully also the project root, where the `docker-compose.yml` file is. If it isn't, well, that sucks ¯\\\_(ツ)_/¯

Next we use [`envsubst`](https://www.man7.org/linux/man-pages/man1/envsubst.1.html) to evaluate any bash variable expression in the [`goban.service`](https://github.com/go-recordkeeper/goban-deploy/blob/ac4628686abe97464fe195d309e2b814270860c9/goban.service) file. I'd never heard of this tool before, but it's pretty neat and works really well.

Finally, rather than piping the output directly to `/etc/systemd/system/goban.service`, we write it first to a temporary file. I did this because `/etc/` is owned by `root`, and for some reason `sudo echo "foo" > /etc/foo` encounters a `Permission Denied` error. I would have assumed that the sudo would also apply to output piping, but what do I know. Using an intermediary temporary file is considerably more verbose, but not harmful, so ¯\\\_(ツ)_/¯

# Deploying

Deployment was as easy as:
```sh
./install-service.sh
# Force systemd to check for new service files
sudo systemctl daemon-reload
# Tell systemd to start the service on boot
sudo systemctl enable goban
# Start the service
sudo systemctl start goban
```

And now the app comes back up momentarily whenever the server is rebooted! (It actually takes a couple of minutes which kinda sucks, but the Raspberry Pi is indeed very small and we should thank it for doing its best.)
