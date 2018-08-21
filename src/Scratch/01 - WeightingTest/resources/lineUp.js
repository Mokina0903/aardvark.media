function init (theId) {
	//var self = $(#theId)
	$('.mainTable').scroll(function() {
		$('.mainTBody').width($('.mainTable').width() + $('.mainTable').scrollLeft());
	});
}