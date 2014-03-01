var mikiApp = angular.module('mikiApp', [
  'ngCookies',
  'ngRoute',
  'ui.bootstrap',
  'diacriticsFilters',
  'mikiControllers'
]).run(function($rootScope, $modal, $http, $cookieStore, appStatus) {
  $rootScope.logged = ($cookieStore.get('token') != undefined);
  $rootScope.$watch(function() { return $cookieStore.get('token'); }, function(newValue) {
    // TODO verify token
    $rootScope.logged = ($cookieStore.get('token') != undefined);
    $rootScope.token = $cookieStore.get('token');
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
  };

  $rootScope.$on('$routeChangeSuccess', function () {
    appStatus.check($rootScope);
  })
}).factory('appStatus', function($cookieStore, $http, $window){
  return {
    check: function(scope) {
      $http.get('/users', {cache: false}).success(function (data, status, headers, config) {
        if(data.length === 0) {
          $window.location.href = "/config";
        }
      });
    }
  };
});

mikiApp.directive('ngMarkdown', function () {
  marked.setOptions({
    renderer: new marked.Renderer(),
    gfm: true,
    tables: true,
    breaks: false,
    pedantic: false,
    sanitize: true,
    smartLists: true,
    smartypants: false,
    highlight: function (code) {
      return hljs.highlightAuto(code).value;
    }
  });
  return {
    restrict: 'AE',
    link: function (scope, element, attrs) {
      if (attrs.ngMarkdown) {
        scope.$watch(attrs.ngMarkdown, function (newVal) {
          var html = newVal ? marked(newVal) : '';
          element.html(html);
        });
      } else {
        var html = marked(element.text());
        element.html(html);
      }
    }
  };
});

mikiApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
    when('/index', {
      templateUrl: '/static/partial/index.html',
      controller: 'IndexCtrl'
    }).
    when('/page', {
      templateUrl: '/static/partial/page_list.html',
      controller: 'PageListCtrl'
    }).
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
      redirectTo: '/index'
    });
  }
]);

