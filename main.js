lastLvl = 0;
noanim  = true;

function onPrevClicked () {
  lastLvl = game.currentLevel;
  game.prev ();
  loadLevel ();
}

function onNextClicked () {
  lastLvl = game.currentLevel;
  game.next ();
  loadLevel ();
}

function loadLevel () {
  const cont = document.querySelector('main > #content');

  const start = new Date().getTime();
  cont.classList.remove('loaded');

  game.getLevel (str => {
    if (md5(str) !== hashes[game.currentLevel]) {
      noanim = true;
      game.currentLevel = lastLvl;
      game.unsolve()
      loadLevel();
      return;
    }

    const f = _ => {
      cont.innerHTML = str;
      cont.classList.add('loaded')
    };

    if (noanim) {
      f ();
      noanim = false;
    } else {
      setTimeout(f,  300 - (new Date().getTime() - start));
    }
  });

  const el = document.querySelector("#level");
  el.innerText = game.currentLevel + 1;

  if (game.isSolved()) {
    el.classList.remove('unsolved');
    el.classList.add('solved');
  } else {
    el.classList.remove('solved');
    el.classList.add('unsolved');
  }


  const prv = document.querySelector("#prv");
  prv.disabled = game.currentLevel === 0;

  const nxt = document.querySelector("#nxt");
  nxt.disabled = game.numSkips() >= 2 && !game.isSolved();
}

window.addEventListener('load', _ => {
  loadLevel ();

  const tb = document.querySelector('#key');
  tb.addEventListener ('keyup', e => {
    if (e.key !== "Enter") return;

    const str = tb.value;
    const sol = [];
    for (let i in str) sol.push(str.charCodeAt(i));

    tb.value = "";
    game.solve(sol);
    onNextClicked();
  });
});

