(function() {
  var Alloc, L, topLevelGroupings,
    __slice = [].slice;

  topLevelGroupings = [
    {
      name: "Priority",
      alloc: [
        {
          cnt: 57
        }, {
          name: "High",
          id: 1,
          cnt: 20
        }, {
          name: "Medium",
          id: 2,
          cnt: 17
        }, {
          name: "Low",
          id: 3,
          cnt: 4
        }
      ]
    }, {
      name: "Version",
      alloc: [
        {
          cnt: 57 + 20 + 17 + 4 - 63 + 1
        }, {
          name: "v2.x",
          id: 20,
          cnt: 60
        }, {
          name: "v3.x",
          id: 21,
          cnt: 0
        }, {
          name: "Defer",
          id: 22,
          cnt: 3
        }
      ]
    }
  ];

  Object.prototype.Ψ = function() {
    var args, fn;
    fn = arguments[0], args = 2 <= arguments.length ? __slice.call(arguments, 1) : [];
    return fn.apply(null, [this].concat(__slice.call(args)));
  };

  L = _;

  Alloc = {
    filterUn: function(as) {
      return L.filter(as, function(a) {
        return !a.name;
      });
    },
    sum: function(as) {
      return L.reduce(as, (function(s, a) {
        return s + a.cnt;
      }), 0);
    }
  };

  angular.module("myApp", []).controller("myCtrl", function($scope) {
    $scope.groupings = topLevelGroupings;
    $scope.countUnAp = function(g) {
      return Alloc.sum(Alloc.filterUn(g.alloc));
    };
    return $scope.countAllAp = function(g) {
      return Alloc.sum(g.alloc);
    };
  });

}).call(this);
