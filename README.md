# Reader Reducer

A small test recreation of some of the Redux architecture using Reader/Writer monads to allow different reducers access to global state when required, and using the writer to build up a log of actions that have happened.

If for some insane reason you want to install this, then:

```sh

git clone https://github.com/danieljharvey/reader-reducer

cd reader-reducer

stack build

stack exec reader-reducer-exec

```

FYI, yes, it probably doesn't make sense to use Reader for the global state access, should really push each whole-data change into State instead so the global state accessed by successive actions is up to date rather than the one at the start of a chain of actions but to be honest I was just happy the thing compiled at all.
