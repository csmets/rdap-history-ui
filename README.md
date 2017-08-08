# A user interface for RDAP history

> Live demo at https://apnic-net.github.io/rdap-history-ui/

This project contains a simple user interface for an [RDAP history](https://github.com/APNIC-net/rdap-history/) [server](https://github.com/APNIC-net/whowas-service/).  It's not meant to be a final product, merely a demonstrator.

### Building

You will need a relatively recent version of NodeJS, preferably installed in an environment.  Then, run:

    npm install

And if you like:

    npm start

To build:

	npm run build

### Coding guide

The interface is written in [Elm](https://elm-lang.org/), so if you're not familiar with Elm the tutorials there are a good starting point.

 - [Main.elm](src/Main.elm) is the main entrypoint, managing the model updates
 - [Model.elm](src/Model.elm) describes the run-time data model of the app
 - [Decode.elm](src/Decode.elm) decodes an RDAP history response into the model
 - [Rdap.elm](src/Rdap.elm) interprets and renders RDAP objects into HTML
 - [Render.elm](src/Render.elm) renders the non-RDAP portions of the interface
 - [Timeline.elm](src/Timeline.elm) is a stub attempt to render a timeline of versions
