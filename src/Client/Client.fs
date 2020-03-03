module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Child

open Shared

type Msg =
    | ChildMsg of Child.InternalMsg
    | ShowPopup
    | ChangeColor


type Model =
    { child: Child.Model
      color: string
      popupShowing: bool }

let childTranslator =
    Child.translator
        { onInternalMessage = ChildMsg
          onShowPopup = ShowPopup }

let init() =
    { child = Child.init
      color = "blue"
      popupShowing = false }, Cmd.none

let changeColorLink dispatch = div [ OnClick(fun _ -> dispatch ChangeColor) ] [ str "Click to change bg color" ]

let popup = div [] [ str "A popup" ]

let switchColor color =
    if color = "red" then "blue" else "red"

let update msg model =
    match msg with
    | ChildMsg internalMsg ->
        let (child', cmd) = Child.update internalMsg model.child
        { model with child = child' }, Cmd.map childTranslator cmd

    | ChangeColor -> { model with color = switchColor model.color }, Cmd.none

    | ShowPopup -> { model with popupShowing = true }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    div []
        [ (Child.view model.child (childTranslator >> dispatch))
          if model.popupShowing then popup else (changeColorLink dispatch) ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
