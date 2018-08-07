angular
    .module("movieApi")
    .config(['$locationProvider', '$routeProvider',
             function config($locationProvider, $routeProvider) {
                 $locationProvider.hashPrefix('!');
                 
                 $routeProvider
                     .when('/apiList', {
                         template: '<api-list></api-list>'
                     })
                     .when('/apiList/theaters', {
                         template: '<api-list></api-list>'
                     })
                     .otherwise('/apiList');
             }
            ]);
