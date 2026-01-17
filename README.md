# Freecell

(Play it on itch.io!)[https://jfeaver.itch.io/freecell]

## Development

I use Elm for my local Elm server. It's great for hot code reloading. You'll
need to download and set that up separately. If you do then you can run this
app using the shell functions in `rc`:

```
source rc
server
```

You can also build an `index.html`:

```
build
```

To upload a working game to itch.io, I copy the `assets` directory and the
built `index.html` into a `manual-build` folder and then compress that
to a zip file and upload it to itch.io.

### ManualTest.elm

I find it helpful to play with a very limited game occasionally. To set this up:

```
cp src/ManualTest.elm.example src/ManualTest.elm
```

Now edit the test file to start the kind of game helps you with manual testing.

Finally, run the app from the command line (assuming you've sourced the `rc`
file earlier):

```
debug
```
