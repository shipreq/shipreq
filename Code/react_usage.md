Guidelines for effective React usage
====================================

* DON'T use Px in props of a component with shouldComponentUpdate defined.
  It isn't checkable by shouldComponentUpdate.
  This will create a bug when the Px updates but the UI doesn't.

* DON'T use Px in state of a component.
  It makes a hidden dependency which will not be checkable in shouldComponentUpdate.
  This will create a bug when the Px updates but the UI doesn't.

* Should I use `Px[A]` in static props or `A` in dynamic props?
  If using `shouldComponentUpdate`, use dynamic props else changes won't be detectable and component may go stale.
  If not using `shouldComponentUpdate` and you plan to re-Px them, use static props as it's a little more effecient.

