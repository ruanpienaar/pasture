
(function(){
  var app = angular.module('pastureApp', []);
  app.controller('PastureController', function($scope, $http){

    $scope.myData = {};

    var response = $http.get("http://localhost:8001/pasture_event");

    response.success(function(data, status, headers, config) {
        $scope.myData.prev          = data.prev;
        $scope.myData.next          = data.next;
        $scope.myData.pasture_event = data.pasture_event;
    });

    response.error(function(data, status, headers, config) {
        alert("rest call failed!");
    });

    $scope.myData.load = function(id) {
        var response = $http.get("http://localhost:8001" + id);

        response.success(function(data, status, headers, config) {
            $scope.myData.prev          = data.prev;
            $scope.myData.next          = data.next;
            $scope.myData.pasture_event = data.pasture_event;
        });
        response.error(function(data, status, headers, config) {
            alert("rest call failed!");
        });


    }

  });
})();