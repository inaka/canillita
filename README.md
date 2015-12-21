#canillita

Simple Paperboy-themed PubSub on top of REST+SSE.

You can find a blog post about this app somewhere in [Inaka's Blog](http://inaka.net/blog) :)

## :warning: DISCLAIMER :warning:
This is canillita **v2**, if you're looking for **v1** switch to branch [v1](https://github.com/inaka/canillita/tree/v1) or tag [1.0](https://github.com/inaka/canillita/tree/1.0) :)

### Introduction
**Canillita** is a RESTful API that allow us to manage `news` by `newspaper` and every time a `news item` is created it gets notified to the listeners.

### Tests
You can run the tests executing `make tests` in the root folder of the app.

### The Server
In order to execute this application you need to execute the following commands:

```
# Start Erlang shell for this project
$ make shell
# Erlang shell started
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:10] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> application:ensure_all_started(canillita).
2>
```
After that you're just good to go.

### RESTful API
After starting the server you can go to `http://localhost:4892/api-docs` and it will give you a nice and practical documentation for Canillita thanks to `cowboy-swagger` and `cowboy-trails`.

---

## Contact Us
For **questions** or **general comments** regarding the use of this project, please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this project, please [open an issue](https://github.com/inaka/canillita/issues/new) in this repo (or a pull request :\)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)
