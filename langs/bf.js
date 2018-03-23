languages.register(new function () {
  this.name = "brainfuck";

  const parse = code => {
    let tree = [];

    // parse it
    for (let i = 0; i < code.length; i++) {
      const c = code[i];

      if (c === '+' || c === '-') {
        let sum = 0;
        for (; (code[i] === '+' || code[i] === '-') && i < code.length; i++)
          sum += code[i] === '+' ? 1 : -1;
        i--;
        if (sum === 0) continue;
        tree.push ({add: sum});
      } else if (c === '<' || c === '>') {
        let sum = 0;
        for (; (code[i] === '<' || code[i] === '>') && i < code.length; i++)
          sum += code[i] === '>' ? 1 : -1;
        i--;
        if (sum === 0) continue;
        tree.push ({move: sum});
      } else if (c === '[') {
        let v   = 1;
        const o = ++i;

        for (; v > 0; i++) {
          if      (code[i] === '[') { v++; }
          else if (code[i] === ']') { v--; }
        }

        tree.push ({loop: parse(code.substr(o, i-o-1))});
        i--;
      } else if (c === ',') { tree.push ({in:  0});
      } else if (c === '.') { tree.push ({out: 0});
      }
    }

    // Quick and dirty optimization pass
    while (true) {
      const _tree = [];
      let i = 0;
      let succ = false;

      for (; i < tree.length-1; i++) {
        const a = tree[i]
            , b = tree[i+1];

        if ('add' in a && 'add' in b) {
          a.add += b.add;
          if (a.add !== 0) _tree.push(a);
          succ = true; i++; break;
        } else if ('move' in a && 'move' in b) {
          a.move += b.move;
          if (a.move !== 0) _tree.push(a);
          succ = true; i++; break;
        } else if ('loop' in a && 'loop' in b) {
          _tree.push(a);
          succ = true; i++; break;
        }

        _tree.push(a);
      }

      if (!succ) break;
      tree = _tree.concat(tree.slice(i+1));
    }

    return tree;
  };

  this.load = code =>
    parse(code.replace(/[^.,+-<>[\]]/g, ''));

  this.interpret = (tree, inp) => {
    if (!tree) return;

    let ix = 0;
    const pop = () => ix > inp.length? 0 : inp[ix++];

    const out = [];
    const mem = new Uint8Array(30*1000);
    let   ptr = 0;

    const f = ops => {
      for (let op of ops) {
        if      ('add'  in op) { mem[ptr]+=op.add; }
        else if ('move' in op) { ptr+=op.move; }
        else if ('loop' in op) { while (mem[ptr] !== 0) f(op.loop); }
        else if ('in'   in op) { mem[ptr] = pop(); }
        else if ('out'  in op) { out.push(mem[ptr]); }
      }
    };

    f (tree);
    return out;
  };
});
