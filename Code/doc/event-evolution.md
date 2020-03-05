Don't forget that events aren't just for server/client comms.
People's project are stored as event streams.
You can't remove an event, or alter an event in a backward-incompatible manner.
Hence the need for this guide.


Adding a new event
==================
Add the event. Make scalac happy. Done.


Making a backward-compatible change to an event
===============================================
Just do it *but* make sure that you follow the [codec evolution guide](codec-evolution.md)
such that protocol versions increase by `.1`.


Making a backward-incompatible change to an event
=================================================
1. Delete existing event.
2. Add new event.


Deleting an new event
=====================

* If a `GenericData` is no longer needed...
  * move the relevant generic data from `GenericData.scala` to `RetiredGenericData.scala`
  * remove its definition from `bin/gen-generic_data`
  * run `bin/gen-generic_data`

* If a `GenericData` is changing...
  * move the relevant generic data from `GenericData.scala` to `RetiredGenericData.scala` and
    append `v1` or similar to the `object` name
  * update its definition in `bin/gen-generic_data`
  * run `bin/gen-generic_data`

* The event itself
  1. Use IDE to rename: append `V1` or similar
  2. Make sure everything compiles
  3. Change to `extends RetiredEvent`
  4. If it uses generic data, change it to use the version in `RetiredGenericData`
