module Child

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json

type OutMsg = ShowPopup

type InternalMsg =
    | Increment
    | Decrement

type Model = int

type Msg =
    | Self of InternalMsg
    | Parent of OutMsg

type TranslationDictionary<'msg> =
    { onInternalMessage: InternalMsg -> 'msg
      onShowPopup: 'msg }

type Translator<'msg> = Msg -> 'msg

let init = 0

let translator dict msg =
    match msg with
    | Self internalMsg -> dict.onInternalMessage internalMsg
    | Parent ShowPopup -> dict.onShowPopup

let update msg model =
    match msg with
    | Increment -> (model + 1), Cmd.none
    | Decrement -> (model - 1), Cmd.none

let view (model: Model) dispatch =
    div []
        [ button [ OnClick(fun _ -> dispatch (Self Increment)) ] [ str "increment" ]
          p [] [ str (model.ToString()) ]
          button [ OnClick(fun _ -> dispatch (Self Decrement)) ] [ str "decrement" ]
          hr []
          button [ OnClick(fun _ -> dispatch (Parent ShowPopup)) ] [ str "show me a popup" ] ]
