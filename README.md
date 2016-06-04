### Elmütt

Elmütt ([screenshot](https://raw.githubusercontent.com/donny/elmutt/master/screenshot.png)
) is a fresh clone of [IdeaBoardz](http://www.ideaboardz.com) written in [Elm](http://elm-lang.org).

[![Deploy](https://www.herokucdn.com/deploy/button.svg)](https://heroku.com/deploy)

#### Development Stack

*Front End*: Elm 0.17 and Bootstrap 4

*Back End*: Python 2.7, Flask, Redis, and WebSocket.

#### Local Back End

Please follow the guide below for running the Python back end locally:

```
redis-server /usr/local/etc/redis.conf
cat 'REDIS_URL=redis://localhost:6379' > .env
heroku local
```

#### To Do:

- [ ] Implement card and list deletion
- [ ] Implement card sorting
- [ ] Implement drag and drop
