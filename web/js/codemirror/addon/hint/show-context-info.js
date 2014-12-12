(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror) {
  var contextInfo = null;

  CodeMirror.attachContextInfo = function(data) {
    CodeMirror.on(data, 'select', function(completion, hints) {
      hints = hints.parentNode;
      var information = null;
      if (completion.info) {
        information = completion.info(completion);
      }
      if (information) {
        var box = hints.getBoundingClientRect();
        if (contextInfo == null) {
          contextInfo = document.createElement('div');
          contextInfo.className = 'CodeMirror-hints-contextInfo'
          document.body.appendChild(contextInfo);
        }
        contextInfo.innerHTML = '';
        contextInfo.style.top = hints.style.top;
        contextInfo.style.left = box.right + 'px';
        if(typeof information == "string") {
          contextInfo.innerHTML = information;
        } else {
          contextInfo.appendChild(information);
        }
        contextInfo.style.display = 'block';
      } else {
        if (contextInfo != null) {
          contextInfo.innerHTML = '';
          contextInfo.style.display = 'none';
        }
      }
    });

    CodeMirror.on(data, 'close', function() {
      if (contextInfo != null) {
        contextInfo.parentNode.removeChild(contextInfo);
      }
      contextInfo = null;
    });

  }

  CodeMirror.showContextInfo = function(getHints) {
    return function(cm, showHints, options) {
      if (!options)
        options = showHints;
      var data = getHints(cm, options);
      CodeMirror.attachContextInfo(data);
      return data;
    }
  }

});
