var mikiControllers = angular.module('mikiControllers', []); 

mikiControllers.controller('PageCtrl', function ($rootScope, $scope, $http, $routeParams) {
  $scope.page_content = "# Loading...";
  if($routeParams.name) {
    $http.get('/pages/' + $routeParams.name, {cache: false}).success(function (data, status, headers, config) {
      $scope.page_content = data;
    }).error(function (data, status, headers, config) {
      $scope.page_content = "# Error " + status;
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

mikiControllers.controller('PageListCtrl', function ($rootScope, $scope, $http) {
  $rootScope.can_edit = false;
  $rootScope.can_create = $rootScope.logged;

  $http.get('/pages', {cache: false}).success(function (data, status, headers, config) {
    $scope.pages = [];
    div = 3;
    len = data.length;
    i = 0;
    do {
      current = i % div;
      if($scope.pages[current] == undefined) {
        $scope.pages[current] = [];
      }
      $scope.pages[current].push(data[i]);
    } while(i++ < len);
  }).error(function(data, status, headers, config) {
    $scope.pages = [];
    $scope.alerts = [{type: 'danger', msg: "Can't retrieve page list!"}];
  });
});

mikiControllers.controller('AdminCtrl', function ($rootScope, $scope, $location, $http) {
  if($rootScope.logged == false) {
    $location.path("/");
  }
  $rootScope.can_edit = false;
  $rootScope.can_create = $rootScope.logged;

  $scope.$watch(function() { return $rootScope.logged; }, function(newValue) {
    if(newValue == false) {
      $location.path("/");
    }
  });

  $scope.alerts = [];
  $scope.closeAlert = function(index) {
    $scope.alerts.splice(index, 1);
  };

  function get_user_list() {
    $http.get('/users', {cache: false}).success(function (data, status, headers, config) {
      $scope.users = data;
    }).error(function(data, status, headers, config) {
      $scope.users = [];
      $scope.alerts = [{type: 'danger', msg: "Can't retrieve user list!"}];
    });
  }
  get_user_list();

  function get_page_list() {
    $http.get('/pages', {cache: false}).success(function (data, status, headers, config) {
      $scope.pages = data;
      console.log($scope.pages);
    }).error(function(data, status, headers, config) {
      $scope.pages = [];
      $scope.alerts = [{type: 'danger', msg: "Can't retrieve page list!"}];
    });
  }
  get_page_list();

  $scope.delete_user = function(username) {
    $http({
      url: '/users/' + username + '/' + $rootScope.token,
      method: "DELETE"
    }).success(function(response) {
      $scope.alerts = [{type: 'success', msg: "User "+username+" removed!"}];
      get_user_list();
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Can't remove user "+username+"!"}];
    });
  };

  $scope.change_password = function() {
    old_password = $scope.old_password;
    new_password = $scope.new_password;

    if(old_password == undefined || old_password == '' || new_password == undefined || new_password == '') {
      $scope.alerts = [{type: 'danger', msg: "Enter old and new password!"}];
      return;
    }

    $http.get("/users/" + $rootScope.token).success(function(data, status, headers, config){
      change_data = { 
        "username": data.ok, "password": old_password, 
        "new_password": new_password, "action": "new_password" 
      };

      $http({
        url: "/users",
        method: "PUT",
        data: change_data,
        headers: {'Content-Type': 'application/json'}
      }).success(function(response) {
        $scope.old_password = "";
        $scope.new_password = "";
        $scope.alerts = [{type: 'success', msg: "Password successfully changed!"}];
      }).error(function(response) {
        $scope.alerts = [{type: 'danger', msg: "Failed to change your password!"}];
      });
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Failed to change your password!"}];
    });
  };

  $scope.add_user = function() {
    username = $scope.user_login;
    password = $scope.user_password;

    if(username == undefined || username == '' || password == undefined || password == '') {
      $scope.alerts = [{type: 'danger', msg: "Enter a username and password to create a new user!"}];
      return;
    }

    $http({
      url: '/users',
      method: "POST",
      data: { "username": username, "password": password, "token": $rootScope.token },
      headers: {'Content-Type': 'application/json'}
    }).success(function(response) {
      $scope.user_login = "";
      $scope.user_password = "";
      $scope.alerts = [{type: 'success', msg: "User "+username+" added!"}];
      get_user_list();
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Faild to add user!"}];
    });
  };

  $scope.delete_page = function(page) {
    $http({
      url: '/pages/' + page + '/' + $rootScope.token,
      method: "DELETE"
    }).success(function(response) {
      $scope.alerts = [{type: 'success', msg: "Page "+page+" removed!"}];
      get_page_list();
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Can't remove page "+page+"!"}];
    });
  };
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
    $http.get('/pages/' + $scope.page_title, {cache: false}).success(function (data, status, headers, config) {
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
      url: '/pages',
      method: "POST",
      data: { "title": $scope.page_title, "content": $scope.page_content, "token": $rootScope.token },
      headers: {'Content-Type': 'application/json'}
    }).success(function(response) {
      if(view === true) {
        $location.path("/page/" + $scope.page_title);
      } else {
        $scope.alerts = [{type: 'success', msg: "Page saved!"}];
        $scope.page_exist = true;
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
        $scope.alerts = [{type: 'danger', msg: "Login faild!"}];
      }
    }).error(function(response) {
      $scope.alerts = [{type: 'danger', msg: "Login faild!"}];
    });
  };
  $scope.cancel = function () {
    $modalInstance.close();
  };
};
