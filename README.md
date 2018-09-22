# Reader Reducer

A small test recreation of some of the Redux architecture using Reader/Writer monads to allow different reducers access to global state when required, and using the writer to build up a log of actions that have happened.

If for some insane reason you want to install this, then:

```sh

git clone https://github.com/danieljharvey/reader-reducer

cd reader-reducer

stack build

stack exec reader-reducer-exec

```
