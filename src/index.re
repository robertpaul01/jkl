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
  font: fontT
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
    randomButton(- height * 1)
  ],
  speed: 3.0,
  yOffset: 0,
  score: 0,
  font: Draw.loadFont(~filename="assets/font.fnt", env)
};

let setup = env => {
  Env.size(~width=fWidth, ~height=fHeight, env);
  initialState(env);
};

let generateNewObs = ({obs}, lastY) =>
  List.map(((_, _, flag) as ob) => flag ? randomButton(lastY) : ob, obs);

let drawObs = ({yOffset, obs}, env) =>
  List.iter(
    ((x, y, _)) => Draw.rect(~pos=(x, y + yOffset), ~width, ~height, env),
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

let checkButtonPress = ({yOffset, obs}, pos) =>
  List.exists(
    ((x, y, _)) => y + yOffset >= fHeight - height * 2 && x == pos,
    obs
  ) ?
    Success : Fail;

let draw = ({gameState, font, yOffset, obs, speed, score} as state, env) => {
  Draw.background(Utils.color(~r=190, ~g=190, ~b=190, ~a=255), env);
  drawButtons(state, env);
  let state =
    switch gameState {
    | Start => {...state, gameState: checkStartGame(env) ? Running : Start}
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
        obs:
          gameState == Success ?
            List.mapi(
              (idx, (x, y, _) as ob) =>
                y
                + yOffset >= fHeight
                - height
                * 2
                && x == keyPos
                && idx == List.length(obs)
                - 1 ?
                  (x, y, true) : ob,
              obs
            ) :
            obs,
        gameState:
          List.exists(((_, y, _)) => y + yOffset >= fHeight + height / 4, obs) ?
            Fail : gameState,
        yOffset: yOffset + int_of_float(speed),
        speed: speed +. 0.005
      };
    | Success =>
      let score = score + 1;
      drawObs(state, env);
      Draw.text(~font, ~body=string_of_int(score), ~pos=(0, 0), env);
      let (_, lastY, _) = List.hd(state.obs);
      {
        ...state,
        gameState: Running,
        obs:
          List.fast_sort(
            ((_, ay, _), (_, by, _)) => ay - by,
            generateNewObs(state, lastY)
          ),
        score
      };
    | Fail =>
      Draw.text(
        ~font,
        ~body="Final score: " ++ string_of_int(score),
        ~pos=(0, 0),
        env
      );
      checkStartGame(env) ? {...initialState(env), gameState: Running} : state;
    };
  state;
};

run(~setup, ~draw, ());