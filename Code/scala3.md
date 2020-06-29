* Change Atom & Text to be type aliases using `&` instead of the inheritance hack
  eg. `type ReqTitle = SingleLine & CodeRefs & ...`

* Delete `TaggedTypes` and make all current subtypes (eg. all IDs) opaque types

* Change FieldKeys to be intersection types and get rid of manual fold classes
