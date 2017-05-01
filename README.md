serverman
---------

A tool for automatically installing, configuring and monitoring services on linux servers.

_Disclaimer: This is an experimental project and is not meant to be used in production at all, there are known issues such as lack of ability to scale, so please, read, get inspired and move on :grin:_

# The Idea
 
The idea behind this experiment was to run scripts on servers, not by sending the script/binary to the server and executing it, but rather by simulating
the server on the local (or as we call it, the host) and letting the script run in the simulated environment.

## But why?

Well, the main reason was to avoid writing the configuration scripts in bash. Bash is not very suitable, in my opinion, for writing deterministic and predictable configuration scripts; I'm not saying Bash is bad, it's just hard to write an script that detects the distro, checks lots of stuff, checks files before writing them, does a good job at logging, etc.

Imagine using a language like Haskell for writing these configurations, you can import the functions you need and have a good time handling errors and edge cases, but Bash? not so well.

On the other hand, it's possible to build the binary of a Haskell script, send it to the server and execute it, but it requires handling of library dependencies, anyways, I'm not going that path, it's an experiment, right?

## But how?

How does serverman simulate the server's environment and execute the script on the local machine?

Magic! no but really, there are a few steps to getting this done:

1. Mount the server filesystem on the local machine using SSHFs, **with sudo access**
  * to have sudo access while using SSHFs, and thus being able to modify system configuration files (/etc), you have to pass this option to sshfs: `-o sftp_server=sudo /usr/lib/openssh/sftp-server` or `/usr/lib/ssh/sftp-server` for Debian and Archlinux respectively
2. Fetch the server's environment variables using any method (`ssh ADDR env` works, for example)
3. Execute each and every system command issued by the script using an `ssh` to the server (you could in practice batch the requests to avoid redundancy)
4. Have a way of port forwarding the required ports by a script. Take a MySQL configuration script for example, it requires connecting to a specific port,
in serverman, there is a `usingPort` function which given a required port, returns a forwarded port that the script can use from there on to access that specific port.

Now this is probably not an exhaustive list, there are other factors that need simulation, but these worked for my experiment.

After setting up the environment, the service script is chrooted into the directory (which allows file system modifications to happen as they would normally do), environment variables are temporarily set for it, and it's commands are executed on the server.

# Get Started
Clone and install serverman:

```
git clone https://github.com/mdibaiee/serverman
cd serverman
stack build && stack install
```

# Basic Commands
Serverman runs command and scripts on your local machine unless given a list of remote servers to work on. I will briefly touch on different commands and then tell you how to run them on remote servers.

## List services

```
serverman repository list
```

## Update repository

You can check out the repository [here](https://github.com/mdibaiee/serverman-repository), there are a few services available already such as nginx and vsftpd.
```
serverman repository update
```

## Install a service
```
serverman install <SERVICE_NAME>
serverman install vsftpd
serverman install nginx
```

## Check a service's status (uses systemctl)
```
serverman service status <SERVICE_NAME>
serverman service status vsftpd
```

## Start/Stop a service (systemctl)
```
serverman service start <SERVICE_NAME>
serverman service start vsftpd
serverman service stop <SERVICE_NAME>
serverman service stop vsftpd
```

## Read service logs (systemctl)
```
serverman service logs <SERVICE_NAME>
serverman service logs vsftpd
```

## Run/configure a service

```
serverman <SERVICE_NAME>
serverman <SERVICE_NAME> -h
serverman <SERVICE_NAME> --help
```

## Remote 
Now in order to run the commands remotely, you have to first create a file containing a list of remote servers to connect to, in the format of `username@ip:port`, port defaults to SSH's default port if unspecified.

At first run, serverman asks for the user's password, and then creates an account for itself in the name of `serverman`, giving it root access.

I advice you first test serverman on a virtual machine (QEMU or VirtualBox or anything).

Example file (`~/remote_list`):
```
ubuntu@192.168.1.3
```

Then, append the option `--remote ~/remote_list` to your commands, and they will be run on the server.

## More

See the help entry for more:

```
serverman --help
```
