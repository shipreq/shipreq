topLevelGroupings = [
  {name: "Priority", alloc: [
    {cnt:57},
    {name:"High"  ,id:1,cnt:20},
    {name:"Medium",id:2,cnt:17},
    {name:"Low"   ,id:3,cnt: 4},
  ]},
  {name: "Version", alloc: [
    {cnt:57+20+17+4-63 + 1},
    {name:"v2.x"  ,id:20,cnt:60},
    {name:"v3.x"  ,id:21,cnt:0},
    {name:"Defer" ,id:22,cnt:3},
  ]},
];

Object::Ψ = (fn, args...) -> fn this, args...
L = _

Alloc =
  filterUn: (as) -> L.filter as, (a) -> !a.name
  sum:      (as) -> L.reduce as, ((s,a) -> s + a.cnt), 0

angular.module "myApp", []
.controller "myCtrl", ($scope) ->

  $scope.groupings = topLevelGroupings

  $scope.countUnAp = (g) -> Alloc.sum Alloc.filterUn g.alloc

  $scope.countAllAp = (g) -> Alloc.sum g.alloc
