var mikiControllers = angular.module('mikiControllers', []); 

mikiControllers.controller('PageCtrl', function ($rootScope, $scope, $http, $routeParams) {
  $scope.page_content = "# Loading...";
  if($routeParams.name) {
    $http.get('/page/' + $routeParams.name, {cache: false}).success(function (data, status, headers, config) {
      $scope.page_content = data;
    }).error(function (data, status, headers, config) {
      $scope.page_content = "# Error " + status +"\n\n```\n" + data + "\n```\n";
    });
  }

  $rootScope.can_edit = $rootScope.logged;
  $rootScope.can_create = $rootScope.logged;
  $scope.$watch(function() { return $rootScope.logged; }, function(newValue) {
    $rootScope.can_edit = newValue;
    $rootScope.can_create = newValue;
  });

  $rootScope.page_name = $routeParams.name
});

mikiControllers.controller('AdminCtrl', function ($rootScope, $scope, $location) {
  if($rootScope.logged == false) {
    $location.path("/");
  }
  $rootScope.can_edit = false;
  $rootScope.can_create = $rootScope.logged;
});

mikiControllers.controller('EditCtrl', function ($rootScope, $scope, $location, $http, $routeParams) {
  if($rootScope.logged == false) {
    if($routeParams.name) {
      $location.path("/page/" + $routeParams.name);
    } else {
      $location.path("/");
    }
  }

  $rootScope.can_edit = false;
  $rootScope.can_create = $rootScope.logged;

  if($routeParams.name) {
    $scope.page_title = $routeParams.name;
    $http.get('/page/' + $scope.page_title, {cache: false}).success(function (data, status, headers, config) {
      $scope.page_content = data;
      $scope.page_exist = true;
    }).error(function(data, status, headers, config) {
      $location.path("/page/" + $scope.page_title);
    });
  } else {
    $scope.page_title = ""
    $scope.page_content = "";
    $scope.page_exist = false;
  }

  $scope.go = function() {
    if($scope.page_exist == true) {
      $location.path("/page/" + $scope.page_title);
    } else {
      $location.path("/");
    }
  };

  $scope.$watch(function() { return $rootScope.logged; }, function(newValue) {
    if(newValue == false) {
      $scope.go();
    }
  });

  $scope.save = function(view) {
    $http({
      url: '/page',
      method: "POST",
      data: { "title": $scope.page_title, "content": $scope.page_content },
      headers: {'Content-Type': 'application/json'}
    }).success(function(response) {
      if(view === true) {
        $location.path("/page/" + $scope.page_title);
      } else {
        $scope.alerts = [{type: 'success', msg: "Page saved!"}];
      }
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Faild to save page!"}];
    });
  };

  $scope.alerts = [];
  $scope.closeAlert = function(index) {
    $scope.alerts.splice(index, 1);
  };
});

var AboutCtrl = function($scope, $modalInstance) {
  $scope.ok = function () {
    $modalInstance.close();
  };
};

var LoginCtrl = function($scope, $modalInstance, $cookieStore, $http) {
  $scope.ok = function(login, password) {
    $http({
      url: '/users',
      method: "PUT",
      data: { "username": login, "password": password, "action": "new_token" },
      headers: {'Content-Type': 'application/json'}
    }).success(function(data, status, headers, config) {
      if(data.ok) {
        $cookieStore.put('token', data.ok); 
        $modalInstance.close();
      } else {
        $scope.alerts = [{type: 'danger', msg: "Login faild ("+data.error+")!"}];
      }
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Server error!"}];
    });
  };
  $scope.cancel = function () {
    $modalInstance.close();
  };
};
