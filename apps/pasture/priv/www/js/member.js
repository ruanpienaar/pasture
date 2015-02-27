
(function(){
  var app = angular.module('pastureApp', []);
  app.controller('PastureController', function($scope, $http){

    // first run
    $scope.p_m_data = {};
    var p_e_init_resp = $http.get(window.location.origin + "/pasture_member");
    p_e_init_resp.success(function(data, status, headers, config) {
        $scope.p_m_data.prev          = data.prev;
        $scope.p_m_data.next          = data.next;
        $scope.p_m_data.pasture_member = data.pasture_member;
    });
    p_e_init_resp.error(function(data, status, headers, config) {
        alert("rest call failed!");
    });

    // Prev/Next click
    $scope.p_m_data.scroll = function(id) {
        var response = $http.get(window.location.origin + id);
        response.success(function(data, status, headers, config) {
            $scope.p_m_data.prev          = data.prev;
            $scope.p_m_data.next          = data.next;
            $scope.p_m_data.pasture_member = data.pasture_member;
        });
        response.error(function(data, status, headers, config) {
            alert("rest call failed!");
        });
    }

  });
})();