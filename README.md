# bolingbroke

Quick-and-dirty embedded folsom metrics viewer.

![Screenshot](/Screenshot.png?raw=true)

## Usage

1. Make sure that the `bolingbroke` application is started.
2. Point your browser to `http://localhost:18360/?foo&bar`; it'll display any
   metric containing "foo" or "bar" in the name.

## Example

If you want to replicate the example given in the screenshot:

    $ erl -pa deps/*/ebin -pa ebin -s bolingbroke
    1> {ok, _} = bolingbroke_example:start_link().
