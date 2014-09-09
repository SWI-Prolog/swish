define([ "jquery" ],
       function() {

function unfold() {
	console.log('unfold:' + $(this).text())
	$(this).next().toggleClass('fold')
	$(this).remove()
}

$(document).on('click', '.pl-functor, .pl-infix', function() {
	var p = $(this).parent()
	console.log('toggling struct ' + $(p).text())
	$(p).toggleClass('fold')
	$(p).before('<span class="pl-ellipsis">...</span>').prev().click(unfold)
})

}); // define
