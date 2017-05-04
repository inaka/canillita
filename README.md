#canillita

Simple Paperboy-themed PubSub on top of REST+SSE.

You can find a blog post about this app in our [Inaka's Blog](http://inaka.net/blog/2016/01/04/canillita-your-first-erlang-web-server-V2/) :)

## :warning: DISCLAIMER :warning:
This is canillita **v2**, if you're looking for **v1** switch to branch [v1](https://github.com/inaka/canillita/tree/v1) or tag [1.0](https://github.com/inaka/canillita/tree/1.0) :)

### Introduction
**Canillita** provides a RESTful API that allow us to manage `news` by `newspaper` and every time a `news item` is published, the listeners get a notification with it.

### Tests
You can run the tests executing `rebar3 do dialyzer, ct` in the root folder of the app.

### The Server
In order to run this application you need to execute the following commands:

```
# Create release
$ rebar3 release
# Run server
$ _build/default/rel/canillita/bin/canillita console
```
After that you're just good to go.

Or you can download a compiled app from the latest release.

### RESTful API
After starting the server you can go to `http://localhost:4892/api-docs` and it will give you a nice and practical documentation for Canillita thanks to `cowboy-swagger` and `cowboy-trails`.

---

## Contact Us
For **questions** or **general comments** regarding the use of this project, please use our public [hipchat room](http://inaka.net/hipchat).

If you find any **bugs** or have a **problem** while using this project, please [open an issue](https://github.com/inaka/canillita/issues/new) in this repo (or a pull request :\)).

And you can check all of our open-source projects at [inaka.github.io](http://inaka.github.io)
