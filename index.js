function openNav() {
    document.getElementById("mySidenav").style.width = "250px";
}

function closeNav() {
    document.getElementById("mySidenav").style.width = "0";
}

(function($) {
    var $window = $(window),
    $html = $('html');

    function resize() {
    	if (window.innerWidth < 900) {
       	document.location = "index_mobile.html";
      }
		}

    $window
        .resize(resize)
        .trigger('resize');
})(jQuery);
