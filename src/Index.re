open Reprocessing;

let fHeight = 650;

let fWidth = 600;

let width = fWidth / 3 - 10;

let height = width / 2;

let buffer = 15;

let bottomOffset = fHeight - height;

let posJ = 0;

let posK = width + buffer;

let posL = width * 2 + buffer * 2;

type runningT =
  | Start
  | Running
  | Success
  | Fail;

type stateT = {
  gameState: runningT,
  obs: list((int, int, bool)),
  speed: float,
  yOffset: int,
  score: int,
  font: fontT,
  bg: imageT,
  block: imageT
};

let randomButton = lastY => {
  let rand = Utils.random(~min=0, ~max=3);
  let button =
    switch rand {
    | 0 => (posJ, lastY - height, false)
    | 1 => (posK, lastY - height, false)
    | 2 => (posL, lastY - height, false)
    | _ => assert false
    };
  button;
};

let initialState = env => {
  gameState: Start,
  obs: [
    randomButton(- height * 10),
    randomButton(- height * 9),
    randomButton(- height * 8),
    randomButton(- height * 7),
    randomButton(- height * 6),
    randomButton(- height * 5),
    randomButton(- height * 4),
    randomButton(- height * 3),
    randomButton(- height * 2),
    randomButton(- height)
  ],
  speed: 3.0,
  yOffset: 0,
  score: 0,
  font: Draw.loadFont(~filename="assets/font.fnt", env),
  bg: Draw.loadImage(~filename="assets/bg.png", ~isPixel=true, env),
  block: Draw.loadImage(~filename="assets/block.png", ~isPixel=true, env)
};

let setup = env => {
  Env.size(~width=fWidth, ~height=fHeight, env);
  initialState(env);
};

let generateNewObs = ({obs}, lastY) =>
  List.fast_sort(
    ((_, ay, _), (_, by, _)) => ay - by,
    List.map(((_, _, flag) as ob) => flag ? randomButton(lastY) : ob, obs)
  );

let drawBlock = ({block}, ~pos, ~width, ~height, env) => {
  let (p, _) = pos;
  let texPos = (144, 49);
  let texPos = p == posK ? (160, 49) : texPos;
  let texPos = p == posL ? (176, 49) : texPos;
  Draw.subImage(
    block,
    ~pos,
    ~width,
    ~height,
    ~texPos,
    ~texWidth=16,
    ~texHeight=16,
    env
  );
};

let drawObs = ({yOffset, obs} as state, env) =>
  List.iter(
    ((x, y, _)) =>
      drawBlock(state, ~pos=(x, y + yOffset), ~width, ~height, env),
    obs
  );

let drawButtons = (state, env) => {
  Draw.fill(Utils.color(~r=49, ~g=79, ~b=79, ~a=255), env);
  drawBlock(state, ~pos=(posJ, bottomOffset), ~width, ~height, env);
  drawBlock(state, ~pos=(posK, bottomOffset), ~width, ~height, env);
  drawBlock(state, ~pos=(posL, bottomOffset), ~width, ~height, env);
};

let checkStartGame = env => Env.keyPressed(Space, env);

let rec markPressedOb = (obs: list((int, int, bool)), yOffset, keyPos) =>
  switch obs {
  | [(x, y, _) as ob] =>
    y + yOffset >= fHeight - height * 2 && x == keyPos ? [(x, y, true)] : [ob]
  | [x, ...xs] => [x] @ markPressedOb(xs, yOffset, keyPos)
  | _ => assert false
  };

let checkMissedOb = ({yOffset, obs, gameState}) =>
  List.exists(((_, y, _)) => y + yOffset >= fHeight + height / 4, obs) ?
    Fail : gameState;

let checkButtonPress = ({yOffset, obs}, pos) =>
  List.exists(
    ((x, y, _)) => y + yOffset >= fHeight - height * 2 && x == pos,
    obs
  ) ?
    Success : Fail;

let draw = ({gameState, font, yOffset, obs, speed, score, bg} as state, env) => {
  Draw.background(Utils.color(~r=190, ~g=190, ~b=190, ~a=255), env);
  Draw.subImage(
    bg,
    ~pos=(0, 0),
    ~width=fWidth,
    ~height=fHeight,
    ~texPos=(0, 0),
    ~texWidth=385,
    ~texHeight=216,
    env
  );
  drawButtons(state, env);
  let state =
    switch gameState {
    | Start =>
      Draw.text(~font, ~body="Press space", ~pos=(0, 0), env);
      {...state, gameState: checkStartGame(env) ? Running : Start};
    | Running =>
      drawObs(state, env);
      Draw.text(~font, ~body=string_of_int(score), ~pos=(0, 0), env);
      let (gameState, keyPos) =
        switch (
          Env.keyPressed(J, env),
          Env.keyPressed(K, env),
          Env.keyPressed(L, env)
        ) {
        | (true, false, false) => (checkButtonPress(state, posJ), posJ)
        | (false, true, false) => (checkButtonPress(state, posK), posK)
        | (false, false, true) => (checkButtonPress(state, posL), posL)
        | _ => (Running, (-1))
        };
      {
        ...state,
        obs: gameState == Success ? markPressedOb(obs, yOffset, keyPos) : obs,
        gameState: checkMissedOb({...state, gameState}),
        yOffset: yOffset + int_of_float(speed),
        speed: speed +. 0.002
      };
    | Success =>
      let score = score + 1;
      drawObs(state, env);
      Draw.text(~font, ~body=string_of_int(score), ~pos=(0, 0), env);
      let (_, lastY, _) = List.hd(state.obs);
      {...state, gameState: Running, obs: generateNewObs(state, lastY), score};
    | Fail =>
      Draw.text(
        ~font,
        ~body="Final score: " ++ string_of_int(score),
        ~pos=(0, 0),
        env
      );
      Draw.text(~font, ~body="Press space", ~pos=(0, height), env);
      checkStartGame(env) ? {...initialState(env), gameState: Running} : state;
    };
  state;
};

run(~setup, ~draw, ());