define([ "jquery" ],
       function() {

function unfold() {
	$(this).next().toggleClass('fold')
	$(this).remove()
}

$(document).on('click', '.pl-functor, .pl-infix', function() {
	var p = $(this).parent()
	$(p).toggleClass('fold')
	$(p).before('<span class="pl-ellipsis">...</span>').prev().click(unfold)
})

}); // define
