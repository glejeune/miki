var mikiApp = angular.module('mikiApp', [
  'ngCookies',
  'ngRoute',
  'ui.bootstrap',
  'mikiControllers' 
]).run(function($rootScope, $modal, $http, $cookieStore) {
  $rootScope.logged = ($cookieStore.get('token') != undefined);
  $rootScope.$watch(function() { return $cookieStore.get('token'); }, function(newValue) {
    // TODO verify token
    $rootScope.logged = ($cookieStore.get('token') != undefined);
  });

  $rootScope.can_edit = false;

  $rootScope.miki_version = ""
  $http({
    url: '/',
    method: "GET",
    headers: {'Accept': 'application/json'}
  }).success(function(response) {
    $rootScope.miki_version = "v" + response.version;
  });

  $rootScope.about = function() {
    var aboutModalInstance = $modal.open({
      templateUrl: '/static/modal/about.html',
      controller: AboutCtrl
    });
  };

  $rootScope.login = function() {
    var loginModalInstance = $modal.open({
      templateUrl: '/static/modal/login.html',
      controller: LoginCtrl
    });
  };

  $rootScope.logout = function() {
    $cookieStore.remove('token');
  }
});

mikiApp.directive('markdown', function () {
  var converter = new Showdown.converter();
  return {
    restrict: 'A',
    link: function (scope, element, attrs) {
      var htmlText = converter.makeHtml(element.text());
      element.html(htmlText);
    }
  };
});

mikiApp.directive('ngMarkdown', function () {
  var converter = new Showdown.converter();
  return {
    restrict: 'AE',
    link: function (scope, element, attrs) {
      if (attrs.ngMarkdown) {
        scope.$watch(attrs.ngMarkdown, function (newVal) {
          var html = newVal ? converter.makeHtml(newVal) : '';
          element.html(html);
        });
      } else {
        var html = converter.makeHtml(element.text());
        element.html(html);
      }
    }
  };
});

mikiApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
    when('/page/:name', {
      templateUrl: '/static/partial/page.html',
      controller: 'PageCtrl'
    }).
    when('/admin', {
      templateUrl: '/static/partial/admin.html',
      controller: 'AdminCtrl'
    }).
    when('/edit/:name', {
      templateUrl: '/static/partial/edit.html',
      controller: 'EditCtrl'
    }).
    when('/edit', {
      templateUrl: '/static/partial/edit.html',
      controller: 'EditCtrl'
    }).
    otherwise({
      redirectTo: '/page/index'
    });
  }
]);

