module Main exposing (main)

import Array exposing (Array)
import Browser exposing (Document)
import Browser.Events
import Html exposing (Html)
import Html.Attributes as A
import Markdown exposing (defaultOptions)
import Http
import Json.Decode as Json

type alias Model =
  { slides : Maybe ( Array String )
  , index : Int }
  
type Msg
  = GetSlides
  | RcvSlides (Result Http.Error String)
  | NextSlide
  | PrevSlide
  | NoOp
  
keyToEvent : String -> Msg
keyToEvent str =
  case str of
    "ArrowLeft" -> 
      PrevSlide
    
    "ArrowRight" ->
      NextSlide
      
    _ ->
      NoOp
  
keyDecoder : Json.Decoder Msg
keyDecoder =
  Json.map keyToEvent <| Json.field "key" Json.string
  
getSlides : Cmd Msg
getSlides = 
  Http.send RcvSlides <| Http.getString "slides.md"
  
init : () -> ( Model, Cmd Msg )
init _ = 
  ( Model Nothing 0
  , getSlides
  )

subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch
    [ Browser.Events.onKeyDown keyDecoder
    ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =  
  case msg of
    GetSlides ->
      ( model
      , getSlides
      )
    
    RcvSlides resp ->
      case resp of
        Ok slides ->
          ( { model | slides = Just <| Array.fromList <| String.split "^^^^" slides }
          , Cmd.none
          )
        Err _ ->
          ( { model | slides = Nothing }
          , Cmd.none
          )
      
    NextSlide ->
      ( { model | index = 
          if model.index + 1 == (Array.length <| Maybe.withDefault Array.empty model.slides) then 
            model.index 
          else 
            model.index + 1 
        }
      , Cmd.none
      )
    
    PrevSlide ->
      ( { model | index = 
        case model.index of
          0 -> 
            0
          n ->
            model.index - 1 
        }
      , Cmd.none 
      )
      
    _ ->
      ( model
      , Cmd.none
      )


getTitle : String -> String
getTitle =
  String.split "\n" 
    >> List.filter (\s -> not <| String.isEmpty s)
    >> List.take 1 
    >> List.head 
    >> Maybe.withDefault "elm-talk"

markdownOptions : Markdown.Options
markdownOptions =
  { defaultOptions 
  | sanitize = False 
  , smartypants = True
  }

view : Model -> Document Msg
view model =
  case model.slides of
    Just md -> 
      let 
        currSlide = 
          Array.get model.index md
          |> Maybe.withDefault ""
      in
        { title = getTitle currSlide
        , body = [ Markdown.toHtmlWith markdownOptions [A.class "slide"] currSlide ]
        }
    Nothing ->
      { title = "404 Slides Not Found"
      , body = [ Html.text "No slides found" ] 
      }

main =
    Browser.document 
      { init = init
      , update = update
      , subscriptions = subscriptions
      , view = view 
      }
