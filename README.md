# Storm Collector

Minimal proof of concept for a device capable of capturing and storing environmental information. Currently tracks temperature and brightness.

Analysis of the data can be done via the [Analyzer application](https://github.com/cloud8421/storm-analyzer).

## Structure

There are two components:

- the sensor device, an Arduino capable of capturing data and sending it to a server via TCP.
- a collector application (written in Erlang), which stores the data and pairs it with additional data coming from the [forecast.io](http://forecast.io/) api.

## The Sensor Device

The device can be built with an Arduino Uno and an Ethernet Shield, plus some other components.

Shopping list, schematics and source code can be found in `/arduino` (you need [Fritzing](http://fritzing.org/) to open the source file).

## Collector application

The application requires Erlang to run.
 
## Setup

Make sure you update the ip address of the collector application server in the Arduino source code.
Once that's done, you can upload your sketch. Some debug information is sent on the serial port, so you wanna keep the debugger open.

The collector application is built using [Rebar](https://github.com/rebar/rebar), so make sure it's installed and available in your `$PATH`.

Make sure you have a forecast.io api key.

Then you can:

    $ rebar get-deps
    $ rebar compile
    $ FORECASTIO_API_KEY=xxx && make start

Then from the erlang shell:
    $ storm_collector_app:start([], []).

This will install the needed dependencies, compile the application and start it.

If everything is setup correctly, you should see data being logged. Data is stored in a DETS table, you can use `storm_collector_storage:all/0` to inspect it.

To inspect the data in json format, you can go to: <http://localhost:8080/datapoints>.

## Customization

Latitude and longitude can be changed in `src/forecast_client.erl`.

The name of the session can be changed in `Makefile`.
