Don't forget that events aren't just for server/client comms.
People's project are stored as event streams.
You can't remove an event, or alter an event in a backward-incompatible manner.
Hence the need for this guide.


Adding a new event
==================

1. Add the event.

2. Update codecs according to the [codec evolution guide](codec-evolution.md)
   This will mean adding codecs for the new event (and possibly new generic data) to the new `RevX` object

3. Make scalac happy but...
   * DON'T CHANGE `ProjectTemplate` IN A BACKWARD-INCOMPATIBLE WAY


Changing an existing event
==========================

The process depends on whether you can do it in a backwards-compatible way,
where backwards-compatible is in terms of the types/data, not the codecs.

Examples:
* adding a new event field           - backwards-compatible
* adding a new GenericData attribute - backwards-compatible
* removing a field                   - not backwards-compatible

#### When backwards-compatible...
* Modify the types/data directly
* Evolve the codecs as described in the [codec evolution guide](codec-evolution.md)

#### When not backwards-compatible...
1. Soft-delete the event (see below)
2. Add a new replacement event (see above)


Deleting an event
=================

1. Use IDE to rename the event: append `V1` or similar

2. Go through `git diff` carefully. Where applicable...
  * similarly rename methods (eg. in `ApplyXxxxEvent`, `RandomData`)
  * fix up alignment

3. Make sure everything compiles

4. `GenericData`:

  * If a `GenericData` is no longer needed...
    * move the relevant generic data from `GenericData.scala` to `RetiredGenericData.scala`
    * remove its definition from `bin/gen-generic_data`
    * run `bin/gen-generic_data`

  * If a `GenericData` is changing...
    * move the relevant generic data from `GenericData.scala` to `RetiredGenericData.scala` and
      append `v1` or similar to the `object` name
    * update its definition in `bin/gen-generic_data`
    * run `bin/gen-generic_data`

5. Change event to extend `RetiredEvent`

6. Change event to use the version of generic data in `RetiredGenericData`

7. Keep the event codecs and retired generic data codecs as they are: just append v1 to all the names

8. Make everything compile