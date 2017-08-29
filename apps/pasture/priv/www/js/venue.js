
(function(){
  var app = angular.module('pastureApp', []);
  app.controller('PastureController', function($scope, $http){

    // first run
    $scope.p_v_data = {};
    var p_e_init_resp = $http.get(window.location.origin + "/pasture_venue");
    p_e_init_resp.success(function(data, status, headers, config) {
        $scope.p_v_data.prev          = data.prev;
        $scope.p_v_data.next          = data.next;
        $scope.p_v_data.pasture_venue = data.pasture_venue;
    });
    p_e_init_resp.error(function(data, status, headers, config) {
        //alert("rest call failed!");
    });

    // Prev/Next click
    $scope.p_v_data.scroll = function(id) {
        var response = $http.get(window.location.origin + id);
        response.success(function(data, status, headers, config) {
            $scope.p_v_data.prev          = data.prev;
            $scope.p_v_data.next          = data.next;
            $scope.p_v_data.pasture_venue = data.pasture_venue;
        });
        response.error(function(data, status, headers, config) {
           // alert("rest call failed!");
        });
    }

  });
})();