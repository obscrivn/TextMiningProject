var repeatFunction = function(){
	var navHeight = $(".navbar-fixed-top").css("height");
	$("body").css("padding-top", navHeight);
};

setInterval(repeatFunction, 1000);