languages = new function () {
  this.reg = {};

  this.register = lang => {
    this.reg[lang.name] = lang;
  };

  this.eval = sc => {
    if ( sc.hasAttribute('data-loaded')
      || !sc.hasAttribute('type')
      || !(sc.type in this.reg)
       ) return;

    sc.setAttribute('data-loaded', '');

    const l   = this.reg[sc.type];
    let token = undefined;

    const f = inp => {
      if (inp instanceof Array) {
        if ("raw" in inp) return f(inp[0]);
        inp = Uint8Array.from(inp);
      } else if (typeof inp === "string") {
        inp = aesjs.utils.utf8.toBytes(inp);
      } else if (inp !== undefined) {
        inp = [inp];
      }

      return l.interpret(token, inp);
    };

    if (sc.hasAttribute('src')) {
      GET (sc.src, data => token = l.load(aesjs.utils.utf8.fromBytes(data)));
    } else {
      token = l.load(sc.text);
    }

    if (sc.hasAttribute('data-name'))
      window[sc.getAttribute('data-name')] = f;

    if (sc.hasAttribute('data-run'))
      f ();
  };

  this.loadAll = () =>
    Array.from(document.scripts).map(languages.eval);
}();
