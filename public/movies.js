//document.domain = "toledo.com";

var config = {
    "base": ""
}

angular
    .module("theApp", [
        "ngRoute",
        "angular-loading-bar"
    ]);

angular
    .module("theApp")
    .config(['$locationProvider', '$routeProvider',
             function config($locationProvider, $routeProvider) {
                 $locationProvider.hashPrefix('!');
                 $routeProvider
                     .when('/theaters', {
                         template: '<theater-list></theater-list>'
                     })
                     .when('/movies', {
                         template: '<all-movies></all-movies>'
                     })
                     .when('/movies/genre/:genre', {
                         template: '<all-movies></all-movies>'
                     })
                     .when('/movie/:id', {
                         template: '<movie-detail></movie-detail>'
                     })
                     .when('/showtimes/:id', {
                         template: '<show-times></show-times>'
                     })
                     .when('/showtimes/:id/:date', {
                         template: '<show-times></show-times>'
                     })
                     .when('/genres', {
                         template: '<genres></genres>'
                     })
                     .otherwise('/theaters');
             }
            ]);

angular
    .module("theApp")
    .component("theaterList", {
        templateUrl: "theater-list.template.html",
        controller: function TheaterList($http, $filter, $sce) {
            var self = this;
            //self.order = "name";
            $http.get(config.base + "/theaters").then(function (response) {
                self.theaters = response.data;
                angular.forEach(self.theaters, function (v, k) {
                    var x = /[0-9]+/.exec(v.th_name);
                    v.screens = x ? parseInt(x[0]) : "";
                });
            });

            self.trustSrc = function(src) {
                return $sce.trustAsResourceUrl(src);
            }

            $http.get(config.base + "/poptrailers").then(function (response) {
                self.poptrailers = response.data;
                //console.log(self.poptrailers);
            });            

        }
    });

angular
    .module("theApp")
    .component("allMovies", {
        templateUrl: "all-movies.template.html",
        controller: function MoviesController($http, $filter, $routeParams) {
            var self = this;
            self.genre = $routeParams.genre;

            self.getGenres = function () {
                $http.get(config.base + "/genres").then(function (response) {
                    self.genres = response.data;
                });
            }
            self.getGenres();

            self.getPopular = function () {
                $http.get(config.base + "/movies/popular").then(function (response) {
                    self.popular = response.data;
                });
            };
            self.getPopular();

            $http.get(config.base + "/movies").then(function (response) {
                self.movies = response.data;
                // console.log(self.movies);
                angular.forEach(self.movies, function (v, k) {
                    angular.forEach(v.shows, function (vv, kk) {
                        angular.forEach(vv.shw_time, function (vvv, kkk) {
                            //console.log(vvv);
                            vv.shw_time[kkk] = moment(vvv).toDate() /* new Date(vvv); */
                        });
                    });
                });
            });
        }
    });

angular
    .module("theApp")
    .component("movieDetail", {
        templateUrl: "movie-detail.template.html",
        controller: function MoviesController($http, $filter, $routeParams, $sce, $location) {
            var self = this;
            var id = $routeParams.id;

            self.trustSrc = function(src) {
                return $sce.trustAsResourceUrl(src);
            }

            $http.get(config.base + "/movie/"+id).then(function (response) {
                self.movie = response.data;
                angular.forEach(self.movie.shows, function (v, k) {
                    angular.forEach(self.movie.shows[k].shw_time, function (vv, kk) {
                        v.shw_time[kk] = moment(vv).toDate(); /* new Date(vv); */
                    });
                });
            });

            self.view = function() {
                //console.log("Okay, here goes your trailer:");
                self.movie.trailer = self.getTrailer(self.movie.trailer.tr_eclipid);
            }

            self.getTrailer = function (id) {
                //console.log(id);
                for (var i=0; i < self.trailers.length; ++i) {
                    var t = self.trailers[i];
                    if (t.tr_eclipid == id) {
                        //console.log(t);
                        return t;
                    }
                }
                return false;
            }

            self.loadTrailers = function () {
                $http.get(config.base + "/trailers/"+id).then(function (response) {
                    self.trailers = response.data;
                });
            }
            self.loadTrailers();
        }
    });

angular
    .module("theApp")
    .component("showTimes", {
        templateUrl: "showtimes.template.html",
        controller: function ShowtimesController($http, $filter, $routeParams, $location) {
            var self = this;
            var id = $routeParams.id;

            self.chosen = $filter('date')(new Date(), "yyyy-MM-dd");
            if (moment($routeParams.date).toDate() != "Invalid Date") {
                self.chosen = moment($routeParams.date).format("YYYY-MM-DD")
            }

            self.update = function () {
                $location.path("/showtimes/"+id+"/"+self.chosen);
                self.loadMovies();
            }

            // self.days = []; 
            // // for (var i = 0; i < 10; ++i) {
            // //     self.days.push(moment().clone().add(i, 'd').toDate());
            // // }

            self.loadDays = function () {
                $http.get("/calendar/"+id).then(function (response) {
                    self.days = response.data;
                    angular.forEach(self.days, function (v1, k1) {
                        //console.log(v1.day);
                        self.days[k1] = new moment(v1.day).toDate();
                        //console.log(self.days[k1]);
                    });
                });
            }
            self.loadDays();
            
            self.loadTheater = function () {
                $http.get(config.base + "/theater/"+id).then(function (response) {
                    self.theater = response.data;
                });
            }
            self.loadTheater();

            self.loadMovies = function () {
                $http.get(config.base + "/showtimes/"+id+"/"+self.chosen).then(function (response) {
                    self.shows = response.data;
                    angular.forEach(self.shows, function (v1, k1) {
                        angular.forEach(v1.shows, function (v2, k2) {
                            //console.log(v2);
                            self.shows[k1].shows[k2] = moment(v2).toDate();
                        });
                    });
                });
            }
            self.loadMovies();
        }
    });

angular
    .module("theApp")
    .component("genres", {
        templateUrl: "genres.template.html",
        controller: function GenresController($http, $filter) {
            var self = this;

            function chunk(arr, size) {
                var newArr = [];
                for (var i=0; i<arr.length; i+=size) {
                    newArr.push(arr.slice(i, i+size));
                }
                return newArr;
            }

            self.getPopular = function () {
                $http.get(config.base + "/movies/popular").then(function (response) {
                    self.popular = response.data;
                });
            };
            self.getPopular();

            // self.chosen = $filter('date')(new Date(), "yyyy-MM-dd");
            // if (moment($routeParams.date).toDate() != "Invalid Date") {
            //     self.chosen = moment($routeParams.date).format("YYYY-MM-DD")
            // }

            // self.days = []; 
            // for (var i = 0; i < 10; ++i) {
            //     self.days.push(moment().clone().add(i, 'd').toDate());
            // }

            self.getGenres = function () {
                $http.get(config.base + "/genres").then(function (response) {
                    self.genres = response.data;
                });
            }
            self.getGenres();

        }
    });
