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
  | Paused
  | Running
  | Fail
  | Restart;

type stateT = {
  gameState: runningT,
  obs: list((float, float)),
  yOffset: float,
  font: fontT,
  mouseDown: bool,
  time: float
};

let setup = (env) => {
  Env.size(~width=fWidth, ~height=fHeight, env);
  {
    gameState: Paused,
    obs: [(float_of_int(posJ), 0.), (float_of_int(posK), (-15.)), (float_of_int(posL), (-40.))],
    yOffset: 2.0,
    font: Draw.loadFont(~filename="assets/font.fnt", env),
    mouseDown: false,
    time: 0.0
  }
};

let generateNewObs = ({obs, yOffset}) =>
  List.map(((x, y)) => (x, y +. yOffset > float_of_int(fHeight) ? (-10.) : y +. yOffset), obs);

let drawObs = ({yOffset, obs}, env) =>
  List.iter(
    ((x, y)) =>
      Draw.rect(~pos=(int_of_float(x), int_of_float(y +. yOffset)), ~width, ~height, env),
    obs
  );

let drawButtons = (state, env) => {
  Draw.fill(Utils.color(~r=49, ~g=79, ~b=79, ~a=255), env);
  Draw.rect(~pos=(posJ, bottomOffset), ~width, ~height, env);
  Draw.rect(~pos=(posK, bottomOffset), ~width, ~height, env);
  Draw.rect(~pos=(posL, bottomOffset), ~width, ~height, env);
  Draw.text(
    ~font=state.font,
    ~body=Env.keyPressed(J, env) ? "J" : "j",
    ~pos=(posJ + width / 2 - buffer, bottomOffset + height / 4),
    env
  );
  Draw.text(
    ~font=state.font,
    ~body=Env.keyPressed(K, env) ? "K" : "k",
    ~pos=(posK + width / 2 - buffer, bottomOffset + height / 4),
    env
  );
  Draw.text(
    ~font=state.font,
    ~body=Env.keyPressed(L, env) ? "L" : "l",
    ~pos=(posL + width / 2 - buffer, bottomOffset + height / 4),
    env
  )
};

let draw = ({gameState, time, font, yOffset} as state, env) => {
  Draw.background(Utils.color(~r=190, ~g=190, ~b=190, ~a=255), env);
  drawButtons(state, env);
  let deltaTime = Env.deltaTime(env);
  let state =
    switch gameState {
    | Paused => {...state, gameState: state.mouseDown ? Running : Paused}
    | Running =>
      Draw.text(~font, ~body=string_of_float(time), ~pos=(0, 0), env);
      drawObs(state, env);
      {...state, obs: generateNewObs(state), time: time +. deltaTime, yOffset: yOffset +. 0.05}
    | Fail => state
    | Restart => state
    };
  {...state, mouseDown: false}
};

let mouseDown = (state, _) => {...state, mouseDown: true};

run(~setup, ~draw, ~mouseDown, ());