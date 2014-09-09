The code is documented using [JsDoc](http://usejsdoc.org/) which, AFAIK,
does not yet provide a nice jQuery plugin. Therefore, jQuery plugins are
represented as classes, while in fact they add a function to `$.fn`.  This
implies that

  - the class `prologEditor` in fact documents to _function_
    `$.fn.prologEditor`.
  - This function can be called in three ways:
    1. As `$(...).prologEditor()`, which is the same as `$(...).prologEditor({})`
    2. As `$(...).prologEditor({...})`, which _initializes_ the jQuery widget
       by calling the method `_init({...})`.  We call this method `_init()` to
       make it appear first in the method list and stress its special meaning.
    3. As `$(...).prologEditor(method, ...)`, which calls the method _method_
       on the widget, together with the provided arguments.  This,

           $("#editor").prologEditor('setSource', src)

       calls the method `setSource(src)` on the jQuery widget instance associated
       with the result of the jQuery expression `$("#editor")`.
