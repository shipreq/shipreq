# UI Prototypes

Here, UI prototypes are written in haml.

Run `./grunt` to convert them into html files in the `render` directory.

Run `make` to watch for changes.

### Workflow

1. Open up the haml file in your editor (left-hand side of screen)
1. Run `make` in the background
1. Uncomment `-# %meta(http-equiv="refresh" content="1")` at the top of the haml
1. Open up the rendered html in your browser (right-hand side of screen)

When finished, comment out `%meta(http-equiv="refresh" content="1")` again.
