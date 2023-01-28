---
title: "Tangent - Hardware Upgrade"
date: 2023-01-27T21:56:35-05:00
type: post
slug: "19-tangent-hardware-upgrade"
tags:
  - manjaro
  - arch
  - git
showTableOfContents: true
---

My velocity on the Haskell implementation has been stalling in favor of other infrastructural work I've been doing on my home network. It's only tangentially related to the go recordkeeper, but it's what I've been doing so that's what I'm going to write about.

# New NAS device!
I recently acquired a [Promethean](https://www.prometheanworld.com/) [Classflow](https://classflow.com/) ActiveConnect PRM-ACON1-01 Mini PC that I've been setting up as a NAS. It's intended use was as a lightweight PC for attaching to a projector or monitor in a classroom setting. It comes with Windows 10, a bunch of ClassFlow apps, and a 64 GB SSD.

I don't care much about the software it ships with, but it's a passively cooled, low power consumption device that can accommodate substantially more storage and processing power than my Raspberry Pis.

# Manjaro 
I'm a Linux fanboy so obviously the first thing I did was to get rid of Windows. Ubuntu is my bread and butter, so I decided to branch out and try [Manjaro](https://manjaro.org/), an [Arch-based](https://archlinux.org/) distribution. I was already using Manjaro on the Raspberry Pi that is hosting the application servers, but basically all I needed to do was install Docker. A NAS offers a more interesting system administration experience. I've heard a lot about Arch/Manjaro, and I've been looking for something suitably substantial to try it out on.

Right away I ran into installation issues setting up `sshd`. Trying to install it with `pacman` led me to a missing `openssl` dependency. Trying to install `openssl` somehow resulted in a partial upgrade which completely broke the system; apparently `openssl` is required to run literally anything from the shell. I spent several hours reinstalling repeatedly and trying to recover from USB.

Thanks to the [Arch wiki](https://wiki.archlinux.org/title/System_maintenance#Partial_upgrades_are_unsupported), I eventually figured out that I needed to run `pacman -Syu`, reboot, rinse and repeat until the upgrade came back clean. After that everything installed smoothly.

Other than that, Manjaro has been quite nice. I've noticed that most online Linux resources tend to assume Debian-based distros and `apt`, but the Arch wiki has been a remarkably useful substitute. I would probably have a harder time digesting it if I wasn't familiar with Linux in general, but with that context it is very informative.

With the server up and accepting SSH logins, I installed some software.

## NFS
I installed and set up an NFS server so that I have a central, backed up location to keep any important files I might have. I haven't set up the backups yet, but that's coming soon.

60 GB is not very much in 2023, but it's more than the miscellaneous files I've accumulated to date, so that's a problem for another day.

## Gitea
I've been a little leery of keeping all my code in GitHub (owned by Microsoft 🤮), so having an alternative that lets me own my own data seemed like a nice idea. [Gitea](https://gitea.io) is indeed very lightweight and easy to install, so I opted for that. I've tried [Gitlab](https://about.gitlab.com/) before, but it is considerably heftier and seems aimed at businesses with robust CI needs.

Gitea has a cool feature where it mirrors a repository by periodically syncing from a remote, so that's what I'm using right now to [back up the go-recordkeeper repository](https://git.chiquit.ooo/daniel/go-recordkeeper). Maybe eventually I'll jettison GitHub entirely, but it is convenient having my work history all in one place and backed up by one of the largest tech monoliths.

You can browse my Gitea at [git.chiquit.ooo](https://git.chiquit.ooo/).

## postgres
Gitea requires a database backend, so I naturally opted to install postgres. Nothing really remarkable about the installation, it went very smoothly.

I plan on eventually using this postgres database to store the application data rather than using the docker compose instance. I felt weird about having "serious" data stored on an SD card on a Raspberry Pi, so this is a good opportunity to centralize all my data onto one machine for less volatility and easier backups. It does make the deployment less contained though.

## nginx
At this point, I had a problem. My residential internet plan gives me a single IP address, but I now have multiple domains to serve: [go.chiquit.ooo](https://go.chiquit.ooo) and [git.chiquit.ooo](https://git.chiquit.ooo). The obvious solution is to use [nginx](https://nginx.org/en/) as a reverse proxy to route the traffic to the correct server within my network.

This was slightly complicated by the fact that I was already using a separate nginx instance as part of the docker compose configuration for the app deployment. That app nginx handled serving static files, parsing URL prefixes to route API requests to the correct implementation, and SSL termination. In order to proxy traffic to the app nginx, the NAS nginx would need to pass HTTPS traffic without doing the SSL termination. Conceptually, this seems possible using the domain of the request, but I could not find a reasonable way to do this in nginx, and it does seem like a bad idea generally.

To resolve this, I just ripped out the SSL termination part of the app deployment. It was nice having a completely contained deployment, but serving the application over HTTP within the host network and having a separately maintained nginx reverse proxy is perfectly normal in the industry.

I thought SSL certificate management would be painful, but the Arch wiki [Certbot entry](https://wiki.archlinux.org/title/Certbot) was very straightforward and helpful. SSL termination is now handled by the NAS level nginx, and the app nginx simply bundles static content and all the implementation URLs into a single domain. There is still a substantial amount of tidying up to do around this, but it's all working.

# Next steps
These probably won't merit a blog post, but they still need doing.

* *Backups*. I'd like to find somewhere outside my house to store backups for locational redundancy. Ideally there would be a rolling backup on a cron job that would compress, encrypt, and upload my NFS directory, the postgres DB, and possibly even the Gitea repositories.
* *Backup config files*. I already use [`configurator`](https://git.chiquit.ooo/daniel/configurator) for managing config files on my laptop, there's no reason it wouldn't work just as well to back up all my configuration files for the NAS. There's certainly less value since I'm not exactly going to be spinning up duplicates, but having backups and having an explicit list of the things I've changed manually will bring some peace of mind.
* *DB migration*. Gotta move the data to the NAS and rip out the old docker compose configuration. Probably not going to do that until the DB is being backed up.
* *nginx cleanup*. I need to tidy up the NAS nginx.conf and the app nginx.conf. I left it rather untidy in my rush to get it working.
* *Update docs*. I need to sift through all my READMEs to make sure they're up-to-date with the new infrastructure.

I will return shortly with your regularly scheduled programming. Tune in next time for more Haskell!
