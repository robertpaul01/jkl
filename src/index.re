open Reprocessing;

let fHeight = 800;

let fWidth = 600;

type runningT =
  | Paused
  | Running
  | Fail
  | Restart;

type stateT = {
  font: fontT,
  gameState: runningT,
  mouseDown: bool,
  time: float
};

let setup = (env) => {
  Env.size(~width=fWidth, ~height=fHeight, env);
  {
    gameState: Paused,
    font: Draw.loadFont(~filename="/assets/font.fnt", env),
    mouseDown: false,
    time: 0.0
  }
};

let drawButtons = (state, env) => {
  let dimen = fWidth / 5;
  let half = dimen / 3;
  let bottomOffset = fHeight - dimen;
  let widthBuffer = fWidth / 8;
  let posJ = widthBuffer;
  let posK = widthBuffer * 3;
  let posL = widthBuffer * 5;
  Draw.fill(Utils.color(~r=49, ~g=79, ~b=79, ~a=255), env);
  Draw.rect(~pos=(posJ, bottomOffset), ~width=dimen, ~height=dimen, env);
  Draw.text(~font=state.font, ~body="j", ~pos=(posJ + half, bottomOffset + 20), env);
  Draw.rect(~pos=(posK, bottomOffset), ~width=dimen, ~height=dimen, env);
  Draw.text(~font=state.font, ~body="k", ~pos=(posK + half, bottomOffset + 20), env);
  Draw.rect(~pos=(posL, bottomOffset), ~width=dimen, ~height=dimen, env);
  Draw.text(~font=state.font, ~body="l", ~pos=(posL + half, bottomOffset + 20), env)
};

let draw = ({gameState, time, font} as state, env) => {
  Draw.background(Utils.color(~r=190, ~g=190, ~b=190, ~a=255), env);
  drawButtons(state, env);
  let deltaTime = Env.deltaTime(env);
  let state =
    switch gameState {
    | Paused => {...state, gameState: state.mouseDown ? Running : Paused}
    | Running =>
      Draw.text(~font, ~body=string_of_float(time), ~pos=(0, 0), env);
      {...state, time: time +. deltaTime}
    | Fail => state
    | Restart => state
    };
  {...state, mouseDown: false}
};

let mouseDown = (state, _) => {...state, mouseDown: true};

run(~setup, ~draw, ~mouseDown, ());