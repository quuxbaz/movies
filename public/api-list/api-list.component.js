angular.
    module("apiList", ["ngRoute"]).
    component("apiList", {
        templateUrl: 'api-list/api-list.template.html',
        controller: ['$http', '$filter', '$routeParams', 
         function ApiListController ($http, $filter, $routeParams) {
             var self = this;
             var date = $filter('date')(new Date(), 'yyyy-MM-dd');
             self.sources = [
                 { name: "List of theaters", url: "/theaters" },
                 { name: "List of all movies", url: "/movies" },
                 { name: "Most popular movies", url: "/movies/popular" },
                 { name: "Trailers of the two most popular movies", url: "/poptrailers" },
                 { name: "A particular movie", url: "/movie/12886620" },
                 { name: "A particular theater", url: "/theater/8856" },
                 { name: "What's showing at Frankling Park today?", url: "/showtimes/5145"},
                 { name: "What's showing at Maumee on $date?", url: "/showtimes/5067/" + date },
                 { name: "Trailers for a movie", url: "/trailers/12706834"},
                 { name: "The calendar for a theater", url: "/calendar/5145" },
                 { name: "A list of genres", url: "/genres" }
             ];
             
             self.setJson = function setJson(data) {
                 self.jsonView = data;
             }
             
             self.showJson = function showJson(url) {
                 $http.get(url).then(function(response) {
                     self.setJson(response.data);
                 });
             }
             
             self.hello = "hello, world!";
         }
        ]
    });
