// CodeMirror, copyright (c) by Marijn Haverbeke and others
// Distributed under an MIT license: http://codemirror.net/LICENSE

(function(mod) {
  if (typeof exports == "object" && typeof module == "object") // CommonJS
    mod(require("../../lib/codemirror"));
  else if (typeof define == "function" && define.amd) // AMD
    define(["../../lib/codemirror", "./prolog-ctype"], mod);
  else // Plain browser env
    mod(CodeMirror);
})(function(CodeMirror, ctype) {
"use strict";

  CodeMirror.defineMode("prolog", function(cmConfig, parserConfig) {

  function chain(stream, state, f) {
    state.tokenize = f;
    return f(stream, state);
  }

		 /*******************************
		 *	   CONFIG DATA		*
		 *******************************/

  var config = { quasiQuotations: true,		/* {|Syntax||Quotation|} */
	         dicts: true,			/* tag{k:v, ...} */
		 unicodeEscape: true,		/* \uXXXX and \UXXXXXXXX */
		 multiLineQuoted: true,		/* "...\n..." */
		 groupedIntegers: true		/* 10 000 or 10_000 */
	       };

  var quoteType = { '"': "string",
		    "'": "qatom",
		    "`": "bqstring"
		  };

  var isSingleEscChar = /[abref\\'"nrtsv]/;
  var isOctalDigit    = /[0-7]/;
  var isHexDigit      = /[0-9a-fA-F]/;

  var isSymbolChar = /[-#$&*+./:<=>?@\\^~]/;	/* Prolog glueing symbols chars */
  var isSoloChar   = /[[\]{}(),;|!]/;		/* Prolog solo chars */
  var isNeck       = /^(:-|-->)$/;
  var isControlOp  = /^(,|;|->|\*->|\\+|\|)$/;


		 /*******************************
		 *	 CHARACTER ESCAPES	*
		 *******************************/

  function readDigits(stream, re, count) {
    if ( count > 0 ) {
      while( count-- > 0 ) {
	if ( !re.test(stream.next()) )
	  return false;
      }
    } else {
      while ( re.test(stream.peek()) )
	stream.next();
    }
    return true;
  }

  function readEsc(stream) {
    var next = stream.next();
    if ( isSingleEscChar.test(next) )
      return true;
    switch( next )
    { case "u":
	if ( config.unicodeEscape )
	  return readDigits(stream, isHexDigit, 4); /* SWI */
        return false;
      case "U":
	if ( config.unicodeEscape )
	  return readDigits(stream, isHexDigit, 8); /* SWI */
        return false;
      case null: return true;			/* end of line */
      case "c": stream.eatSpace(); return true;
      case "x": return readDigits(stream, isHexDigit, 2);
    }
    if ( isOctalDigit.test(next) ) {
      if ( !readDigits(stream, isOctalDigit, -1) )
	return false;
      if ( stream.peek() == "\\" )		/* SWI: optional closing \ */
	stream.next();
      return true;
    }
    return false;
  }

  function nextUntilUnescaped(stream, state, end) {
    var next;
    while ((next = stream.next()) != null) {
      if ( next == end && end != stream.peek() )
      { state.nesting.pop();
        return false;
      }
      if ( next == "\\" )
      { if ( !readEsc(stream) )
	  return false;
      }
    }
    return config.multiLineQuoted;
  }

		 /*******************************
		 *	CONTEXT NESTING		*
		 *******************************/

  function nesting(state) {
    return state.nesting.slice(-1)[0];
  }

  /* Called on every non-comment token */
  function setArg1(state) {
    var nest = nesting(state);
    if ( nest ) {
      if ( nest.arg == 0 )		/* nested in a compound */
	nest.arg = 1;
      else if ( nest.type == "control" )
	state.goalStart = false;
    } else
      state.goalStart = false;
  }

  function setArgAlignment(state) {
    var nest = nesting(state);
    if ( nest && !nest.alignment && nest.arg != undefined ) {
      if ( nest.arg == 0 )
	nest.alignment = nest.leftCol ? nest.leftCol+4 : nest.column+4;
      else
	nest.alignment = nest.column+1;
    }
  }

  function nextArg(state) {
    var nest = nesting(state);
    if ( nest ) {
      if ( nest.arg )			/* nested in a compound */
	nest.arg++;
      else if ( nest.type == "control" )
	state.goalStart = true;		/* FIXME: also needed for ; and -> */
    } else
      state.goalStart = true;
  }

  function isControl(state) {		/* our terms are goals */
    var nest = nesting(state);
    if ( nest ) {
      if ( nest.type == "control" ) {
	return true;
      }
      return false;
    } else
      return state.inBody;
  }

  // Used as scratch variables to communicate multiple values without
  // consing up tons of objects.
  var type, content;
  function ret(tp, style, cont) {
    type = tp; content = cont;
    return style;
  }

  function peekSpace(stream) {		/* TBD: handle block comment as space */
    if ( stream.eol() ||
	 /[\s%]/.test(stream.peek()) )
      return true;
    return false;
  }


		 /*******************************
		 *	   SUB TOKENISERS	*
		 *******************************/

  function plTokenBase(stream, state) {
    var ch = stream.next();

    if ( ch == "(" ) {
      if ( state.lastType == "functor" ) {
	state.nesting.push({ functor: state.functorName,
			     column: stream.column(),
			     leftCol: state.functorColumn,
			     arg: 0
			   });
	delete state.functorName;
	delete state.functorColumn;
      } else {
	state.nesting.push({ type: "control",
			     closeColumn: stream.column(),
			     alignment: stream.column()+4
			   });
      }
      return ret("solo", null, "(");
    }

    if ( ch == "{" && state.lastType == "tag" ) {
      state.nesting.push({ tag: state.tagName,
			   column: stream.column(),
			   leftCol: state.tagColumn,
			   arg: 0
			 });
      delete state.tagName;
      delete state.tagColumn;
      return ret("dict_open", null);
    }

    if ( ch == "/" && stream.eat("*") )
      return chain(stream, state, plTokenComment);

    if ( ch == "%" ) {
      stream.skipToEnd();
      return ret("comment", "comment");
    }

    setArg1(state);

    if ( isSoloChar.test(ch) ) {
      switch ( ch )
      { case ")":
	  state.nesting.pop();
	  break;
	case "]":
	  state.nesting.pop();
	  return ret("list_close", null, "]");
	case "}":
	{ var nest = nesting(state);
	  var type = (nest && nest.tag) ? "dict_close" : "brace_term_close";

	  state.nesting.pop();
	  return ret(type, null);
	}
	case ",":
	  if ( stream.eol() )
	    state.commaAtEOL = true;
	  nextArg(state);
          /*FALLTHROUGH*/
	case ";":
	  if ( isControl(state) )
	    state.goalStart = true;
	  break;
	case "[":
	  state.nesting.push({ type: "list",
			       closeColumn: stream.column(),
			       alignment: stream.column()+2
			     });
	  return ret("list_open", null, "[");
	  break;
	case "{":
	  if ( config.quasiQuotations && stream.eat("|") ) {
	    state.nesting.push({ type: "quasi-quotation",
			         alignment: stream.column()+1
			       });
	    return ret("qq_open", "qq_open");
	  } else {
	    state.nesting.push({ type: "curly",
			         closeColumn: stream.column(),
				 alignment: stream.column()+2
			       });
	    return ret("brace_term_open", null);
	  }
	  break;
	case "|":
	  if ( config.quasiQuotations ) {
	    if ( stream.eat("|") ) {
	      state.tokenize = plTokenQuasiQuotation;
	      return ret("qq_sep", "qq_sep");
	    } else if ( stream.eat("}") ) {
	      state.nesting.pop();
	      return ret("qq_close", "qq_close");
	    }
	  }
	  if ( isControl(state) )
	    state.goalStart = true;
	  break;
      }
      return ret("solo", null, ch);
    }

    if (ch == '"' || ch == "'" || ch == "`")
    { state.nesting.push({ type: "quoted",
			   alignment: stream.column()+1
			 });
      return chain(stream, state, plTokenString(ch));
    }

    if ( ch == "0" ) {
      if ( stream.eat(/x/i)) {
	stream.eatWhile(/[\da-f]/i);
	return ret("number", "number");
      }
      if ( stream.eat(/o/i)) {
	stream.eatWhile(/[0-7]/i);
	return ret("number", "number");
      }
      if ( stream.eat(/'/) ) {			/* 0' */
	var next = stream.next();
	if ( next == "\\" ) {
	  if ( !readEsc(stream) )
	    return ret("error", "error");
	}
	return ret("code", "code");
      }
    }

    if ( /\d/.test(ch) || /[+-]/.test(ch) && stream.eat(/\d/)) {
      if ( config.groupedIntegers )
	stream.match(/^\d*((_|\s+)\d+)*(?:\.\d+)?(?:[eE][+\-]?\d+)?/);
      else
	stream.match(/^\d*(?:\.\d+)?(?:[eE][+\-]?\d+)?/);
      return ret(ch == "-" ? "neg-number" :
		 ch == "+" ? "pos-number" :
		 "number");
    }

    if ( ctype.symbol(ch) ) {
      stream.eatWhile(ctype.symbol);
      var atom = stream.current();
      if ( atom == "." && peekSpace(stream) ) {
	if ( nesting(state) ) {
	  return ret("fullstop", "error", atom);
	} else {
	} return ret("fullstop", "fullstop", atom);
      } else if ( isNeck.test(atom) ) {
	return ret("neck", "neck", atom);
      } else if ( isControl(state) && isControlOp.test(atom) ) {
	state.goalStart = true;
	return ret("symbol", "operator", atom);
      } else
	return ret("symbol", "operator", atom);
    }

    stream.eatWhile(ctype.id_continue);
    var word = stream.current();
    if ( stream.peek() == "{" && config.dicts ) {
      state.tagName = word;			/* tmp state extension */
      state.tagColumn = stream.column();
      return ret("tag", "tag", word);
    } else if ( ch == "_" ) {
      if ( word.length == 1 ) {
	return ret("var", "anon", word);
      } else {
	var sec = word.charAt(1);
	if ( ctype.uppercase(sec) )
	  return ret("var", "var-2", word);
      }
      return ret("var", "var", word);
    } else if ( ctype.uppercase(ch) ) {
      return ret("var", "var", word);
    } else if ( stream.peek() == "(" ) {
      state.functorName = word;			/* tmp state extension */
      state.functorColumn = stream.column();
      return ret("functor", "functor", word);
    } else
      return ret("atom", "atom", word);
  }

  function plTokenString(quote) {
    return function(stream, state) {
      if (!nextUntilUnescaped(stream, state, quote)) {
        state.tokenize = plTokenBase;
	if ( stream.peek() == "(" ) {		/* 'quoted functor'() */
	  var word = stream.current();
	  state.functorName = word;		/* tmp state extension */
	  return ret("functor", "functor", word);
	}
	if ( stream.peek() == "{" && config.dicts ) { /* 'quoted tag'{} */
	  var word = stream.current();
	  state.tagName = word;			/* tmp state extension */
	  return ret("tag", "tag", word);
	}
      }
      return ret(quoteType[quote], quoteType[quote]);
    };
  }

  function plTokenQuasiQuotation(stream, state) {
    var maybeEnd = false, ch;
    while (ch = stream.next()) {
      if (ch == "}" && maybeEnd) {
        state.tokenize = plTokenBase;
	stream.backUp(2);
	break;
      }
      maybeEnd = (ch == "|");
    }
    return ret("qq_content", "qq_content");
  }

  function plTokenComment(stream, state) {
    var maybeEnd = false, ch;
    while (ch = stream.next()) {
      if (ch == "/" && maybeEnd) {
        state.tokenize = plTokenBase;
        break;
      }
      maybeEnd = (ch == "*");
    }
    return ret("comment", "comment");
  }


		 /*******************************
		 *	   RETURN OBJECT	*
		 *******************************/

  return {
    startState: function() {
      return {
        tokenize: plTokenBase,
	inBody: false,
	goalStart: false,
	lastType: null,
	nesting: new Array(),		/* ([{}]) nesting FIXME: copy this */
	curTerm: null,			/* term index in metainfo */
	curToken: null			/* token in term */
      };
    },

    token: function(stream, state) {
      var nest;

      if ( state.curTerm == null && parserConfig.metainfo ) {
	state.curTerm = 0;
	state.curToken = 0;
      }

      if ( stream.sol() )
	delete state.commaAtEOL;

      if ( state.tokenize == plTokenBase && stream.eatSpace() ) {
	if ( stream.eol() )
	  setArgAlignment(state);
	return null;
      }

      var style = state.tokenize(stream, state);

      if ( stream.eol() )
	setArgAlignment(state);

      if ( type == "neck" ) {
	state.inBody = true;
	state.goalStart = true;
      } else if ( type == "fullstop" ) {
	state.inBody = false;
	state.goalStart = false;
      }

      state.lastType = type;

      if ( typeof(parserConfig.enrich) == "function" )
	style = parserConfig.enrich(stream, state, type, content, style);

      return style;
    },

    indent: function(state, textAfter) {
      if (state.tokenize == plTokenComment) return CodeMirror.Pass;

      var nest;
      if ( (nest=nesting(state)) ) {
	if ( nest.closeColumn && !state.commaAtEOL )
	  return nest.closeColumn;
	return nest.alignment;
      }
      if ( !state.inBody )
	return 0;

      return 4;
    },

    theme: "prolog",

    blockCommentStart: "/*",		/* continuecomment.js support */
    blockCommentEnd: "*/",
    blockCommentContinue: " * ",
    lineComment: "%",
  };
});

CodeMirror.defineMIME("text/x-prolog", "prolog");
});
