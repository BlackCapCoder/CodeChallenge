function GET (url, callback) {
  const xhttp = new XMLHttpRequest();
  xhttp.onreadystatechange = function () {
    if (this.readyState != 4) return;
    callback (new Uint8Array(this.response));
  }
  xhttp.open ("GET", url, true);
  xhttp.responseType = "arraybuffer";
  xhttp.send ();
}

Array.prototype.lpad = function (x, n) {
  if (this.length >= n) return this;
  return new Array(n-this.length).fill(x).concat(this);
}

// Read the skipfile
GET ('skipfile', data => {
  skipfile = new function () {
    this.raw = data;

    const fact = n => {
      if (n <= 1) return 1;
      return n * fact (n-1);
    };

    const nPickK = (n, k) => fact (n) / (k * fact (n-k));

    this.histLength = lvl => {
      if (lvl === 0) return 0;
      return 1 + (lvl*lvl+lvl) / 2;
    };

    this.histLengthCum = n => {
      if (n > 0) return n*(n*n+3*n+8) / 6
      return 0
    };

    // Takes an array of ints; 0=Beat, 1=Skip
    this.histToIndex = hs => {
      if (hs.length === 0) return 0;

      const l = hs.length;
      let ret = this.histLengthCum (l-1) + 1;

      const i1 = hs.indexOf(1);
      if (i1 === -1) return ret;
      ret += nPickK (l+1-i1, 2);

      const i2 = hs.slice(i1+1).indexOf(1);
      return ret - ( i2 === -1
                   ? hs.length - i1 - 1
                   : i2
                   );
    };

    this.getSkip = hs => {
      const ix = this.histToIndex(hs)*32;
      return this.raw.subarray(ix, ix+32);
    };

    this.decrypt = (data, key) => {
      if (key instanceof Array) key = new Uint8Array(key.lpad(0, 32));
      const aesCtr = new aesjs.ModeOfOperation.ctr(key, new aesjs.Counter(0));
      return aesCtr.decrypt(data);
    };

    this.decrypts = (data, ks) => {
      let ret = data;
      for (let k of ks) ret = this.decrypt(ret, k);
      return ret;
    };

    this.getSkipDec = (hist, keys) => {
      const s = this.getSkip(hist);
      return this.decrypts(s, keys);
    };
  }();
});

game = new function () {
  this.currentLevel = 0;
  this.history      = []; // 0=beat, 1=skip
  this.solutions    = [];

  this.prev = () => {
    if (this.currentLevel <= 0) return;
    this.currentLevel--;
  };

  this.next = () => {
    if (this.history[this.currentLevel] !== 0)
      this.history[this.currentLevel] = 1;

    this.currentLevel++;
  };

  this.solve = solution => {
    this.solutions[this.currentLevel] = solution;
    this.history[this.currentLevel] = 0;
  };

  this.unsolve = () => {
    delete this.solutions[this.currentLevel];
    this.history[this.currentLevel] = 1;
  }

  this.getLevel = callback => {
    const pth = 'levels-enc/' + (this.currentLevel + 1) + '.html';
    // const pth = 'levels/' + (this.currentLevel + 1) + '.html';

    GET (pth, data => {
      const hist = []
          , keys = [];

      for (let i = 0; i < this.currentLevel; i++) {
        hist.push(this.history[i]);
        if (i in this.solutions) keys.push(this.solutions[i]);
      }

      const key = skipfile.getSkipDec(hist, keys);
      const res = skipfile.decrypt (data, key);
      // const res = data;
      const str = aesjs.utils.utf8.fromBytes(res);

      callback ( str );
    });

    this.isSolved = () => {
      return this.currentLevel in this.solutions;
    };

    this.numSkips = () => {
      let cnt = 0;
      for (let i = 0; i < this.currentLevel; i++)
        if (this.history[i] === 0) cnt++;
      return this.currentLevel - cnt;
    }
  };

}();

GET ('hashes', data => {
  const str = aesjs.utils.utf8.fromBytes(data);
  hashes = str.split('\n');
});
