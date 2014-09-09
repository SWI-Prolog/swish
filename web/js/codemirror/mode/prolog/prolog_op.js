Default (SWI-)Prolog operator table.   To be used later to enhance the
offline experience.

  var ops = { "-->":   { p:1200, t:"xfx" },
	      ":-":    [ { p:1200, t:"xfx" },
			 { p:1200, t:"fx" }
		       ],
	      "?-":    { p:1200, t:"fx" },

	      "dynamic":            { p:1150, t:"fx" },
	      "discontiguous":      { p:1150, t:"fx" },
	      "initialization":     { p:1150, t:"fx" },
	      "meta_predicate":     { p:1150, t:"fx" },
	      "module_transparent": { p:1150, t:"fx" },
	      "multifile":          { p:1150, t:"fx" },
	      "thread_local":       { p:1150, t:"fx" },
	      "volatile":           { p:1150, t:"fx" },

	      ";":    { p:1100, t:"xfy" },
	      "|":    { p:1100, t:"xfy" },

	      "->":   { p:1050, t:"xfy" },
	      "*->":  { p:1050, t:"xfy" },

	      ",":    { p:1000, t:"xfy" },

	      "\\+":  { p:900,  t:"fy" },

	      "~":    { p:900,  t:"fx" },

	      "<":    { p:700,  t:"xfx" },
	      "=":    { p:700,  t:"xfx" },
	      "=..":  { p:700,  t:"xfx" },
	      "=@=":  { p:700,  t:"xfx" },
	      "=:=":  { p:700,  t:"xfx" },
	      "=<":   { p:700,  t:"xfx" },
	      "==":   { p:700,  t:"xfx" },
	      "=\\=": { p:700,  t:"xfx" },
	      ">":    { p:700,  t:"xfx" },
	      ">=":   { p:700,  t:"xfx" },
	      "@<":   { p:700,  t:"xfx" },
	      "@=<":  { p:700,  t:"xfx" },
	      "@>":   { p:700,  t:"xfx" },
	      "@>=":  { p:700,  t:"xfx" },
	      "\\=":  { p:700,  t:"xfx" },
	      "\\==": { p:700,  t:"xfx" },
	      "is":   { p:700,  t:"xfx" },

	      ":":    { p:600,  t:"xfy" },

              "+":    [ { p:500,  t:"yfx" },
			{ p:200,  t:"fy" }
		      ],
              "-":    [ { p:500,  t:"yfx" },
			{ p:200,  t:"fy" }
		      ],
              "/\\":  { p:500,  t:"yfx" },
              "\\/":  { p:500,  t:"yfx" },
              "xor":  { p:500,  t:"yfx" },

              "?":    { p:500,  t:"fx" },

              "*":    { p:400,  t:"yfx" },
              "/":    { p:400,  t:"yfx" },
              "//":   { p:400,  t:"yfx" },
              "rdiv": { p:400,  t:"yfx" },
              "<<":   { p:400,  t:"yfx" },
              ">>":   { p:400,  t:"yfx" },
              "mod":  { p:400,  t:"yfx" },
              "rem":  { p:400,  t:"yfx" },

              "**":   { p:200,  t:"xfx" },
              "^":    { p:200,  t:"xfy" },

              "\\":   { p:200,  t:"fy" }
	    };
