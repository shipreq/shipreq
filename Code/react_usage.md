Guidelines for effective React usage
====================================

* DON'T use Px in props of a component with shouldComponentUpdate defined.
  It isn't checkable by shouldComponentUpdate.
  This will create a bug when the Px updates but the UI doesn't.

* DON'T use Px in state of a component.
  It makes a hidden dependency which will not be checkable in shouldComponentUpdate.
  This will create a bug when the Px updates but the UI doesn't.
