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
  | Fail;

type stateT = {
  gameState: runningT,
  lastY: int,
  obs: list((int, int)),
  yOffset: int,
  font: fontT,
  time: float
};

let randomButton = lastY => {
  let rand = Utils.random(~min=0, ~max=2);
  let button =
    switch rand {
    | 0 => (posJ, lastY - height)
    | 1 => (posK, lastY - height)
    | 2 => (posL, lastY - height)
    | _ => assert false
    };
  button;
};

let initialState = env => {
  gameState: Start,
  lastY: 0,
  obs: [
    randomButton(0),
    randomButton(- height),
    randomButton(- height * 2),
    randomButton(- height * 3),
    randomButton(- height * 4),
    randomButton(- height * 5),
    randomButton(- height * 6),
    randomButton(- height * 7),
    randomButton(- height * 8),
    randomButton(- height * 9),
    randomButton(- height * 10)
  ],
  yOffset: 5,
  font: Draw.loadFont(~filename="assets/font.fnt", env),
  time: 0.0
};

let setup = env => {
  Env.size(~width=fWidth, ~height=fHeight, env);
  initialState(env);
};

let generateNewObs = ({obs, yOffset, lastY}) =>
  List.map(
    ((x, y)) => y + yOffset > fHeight ? randomButton(lastY) : (x, y),
    obs
  );

let drawObs = ({yOffset, obs}, env) =>
  List.iter(
    ((x, y)) => Draw.rect(~pos=(x, y + yOffset), ~width, ~height, env),
    obs
  );

let drawButtons = (state, env) => {
  Draw.fill(Utils.color(~r=49, ~g=79, ~b=79, ~a=255), env);
  Draw.rect(~pos=(posJ, bottomOffset), ~width, ~height, env);
  Draw.rect(~pos=(posK, bottomOffset), ~width, ~height, env);
  Draw.rect(~pos=(posL, bottomOffset), ~width, ~height, env);
  Draw.text(
    ~font=state.font,
    ~body="j",
    ~pos=(posJ + width / 2 - buffer, bottomOffset + height / 4),
    env
  );
  Draw.text(
    ~font=state.font,
    ~body="k",
    ~pos=(posK + width / 2 - buffer, bottomOffset + height / 4),
    env
  );
  Draw.text(
    ~font=state.font,
    ~body="l",
    ~pos=(posL + width / 2 - buffer, bottomOffset + height / 4),
    env
  );
};

let checkStartGame = env =>
  Env.keyPressed(J, env) || Env.keyPressed(K, env) || Env.keyPressed(L, env);

let draw = ({gameState, time, font, yOffset, obs} as state, env) => {
  Draw.background(Utils.color(~r=190, ~g=190, ~b=190, ~a=255), env);
  drawButtons(state, env);
  let deltaTime = Env.deltaTime(env);
  let state =
    switch gameState {
    | Start => {...state, gameState: checkStartGame(env) ? Running : Start}
    | Running =>
      Draw.text(~font, ~body=string_of_float(time), ~pos=(0, 0), env);
      drawObs(state, env);
      {
        ...state,
        obs:
          List.fast_sort(((_, ay), (_, by)) => ay - by, generateNewObs(state)),
        gameState:
          List.exists(((_, y)) => y + yOffset >= fHeight, obs) ?
            Running : Running,
        time: time +. deltaTime,
        yOffset: yOffset + 2
      };
    | Fail =>
      Draw.text(
        ~font,
        ~body="Final time: " ++ string_of_float(time),
        ~pos=(0, 0),
        env
      );
      checkStartGame(env) ? {...initialState(env), gameState: Running} : state;
    };
  {...state, lastY: snd(List.hd(state.obs))};
};

run(~setup, ~draw, ());