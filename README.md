# Mist Connections: Tinder, but for the weather.

> The UI is incomplete. I was going to work in a joke where tapping on the detail for a weather forecast allowed you to "swipe" on the forecast to show approval or disinterest, like on a dating app - but I didn't get that far. ("Mist Connections", get it? Har har har.)

* `start.sh` runs the Python API, watches the `elm-ui` folder for changes and builds-on-write, and opens Safari to http://localhost:5000
* The Javascript function to check for location always fails if the site isn't running on HTTPS. In Chrome, you can load the URL `chrome://flags/#unsafely-treat-insecure-origin-as-secure` and add `http://localhost:5000` as a workaround.
  * The failure mode displays a message that indicates that location is unavailable, and shows you the weather for 1 Infinite Loop, Cupertino, CA instead.
* If you want to just run the application, just run `python3 mist-connections.py` and browse to http://localhost:5000
* The Elm compiler and toolset isn't a runtime dependency - the Elm code compiles to `static/Main.js`. It's currently under source control, but since it's a built asset, if this were a production-ready repository I'd have a deployment script that kept `Main.js` out of source control, built it, and added it to a deployment tag.
  * But for the curious, Elm can be installed from https://elm-lang.org.
* Some of the assets in `static` are unnecessary - I wasn't sure what all I was going to use for the UI, so there are some Javascript files in there that are referenced by the application, but not actually used for anything.
