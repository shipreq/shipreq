Pro Angular.JS
==============

### Chapter 2

* <html ng-app="todoApp">
* var todoApp = angular.module("todoApp", []);
* <body ng-controller="todoCtrl">
* todoApp.controller("todoCtrl", function($scope) { $scope.todo = model; });
* <p>Name is {{todo.name}}</p>
* <tr ng-repeat="i in todo.items"><td>{{i.name}}</td></tr>
* Prefer logic in controller rather than in view interpolation.
* <input type="checkbox" ng-model="i.done" />
* ng-hide="blahGetBool()" hides self and children.
* ng-class="blahGetClass()" adds classes.
* ng-model can use tmp vars, doesn't always need to point to model
* ng-click="doStuff(tmpModelVar)"
* JS: array.push adds new item (mutably of course)
* ng-repeat="i in todo.items | filter: {done:false} | orderBy: 'name'"

### Chapter 5

* JS: d = {a:"A", b:2}; d.c = "C!"; delete d.b;
* JS: "a" in d // == true (containsKey)
* JS: for (var k in d){ console.log(d[k]); }
* JS: Arrays
  * +: unshift() | push()
  * -: shift()   | pop()
  * concat = append
  * join = mkString
* angular.forEach(d, function(v,k){ ... });
* angular.{to,from}Json

### Chapter 6
* `angular.module("x", []);` creates a new module.
* `angular.module("x", ["y"]);` declares module "y" as a dependency.
* `angular.module("x");` locates an existing module.
* `module.filter("name",f1(){ f2(args){...} }` create a custom filter.
* `ng-repeat="i in data.products | orderBy:'name' | customFilter:'name'"` applies a custom filter.
* Transient vars (eg. view state like sort order) are just declares as standard JS vars in the `.controller` function.
  Add related behaviour to `$scope` but not the var itself.
* `angular.module(..).constant(k,v)`
* `ng-repeat` exposes `$index`
