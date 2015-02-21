import Signal
import Time
import Html (..)
import Html.Attributes (..)
import Html.Events (..)

import Array as A

images = 
  [ "/images/1.jpg"
  , "/images/2.jpg"
  , "/images/3.jpg"
  , "/images/4.jpg"
  , "/images/5.jpg"
  ]


type alias Model =
  { images : A.Array String
  , idx : Int
  }

init : Model
init = 
  { images = A.fromList images
  , idx = 0
  }

fromJust : Maybe a -> a
fromJust (Just a) = a

current : Model -> (String,String)
current m =
  ( fromJust (A.get m.idx m.images)
  , fromJust (A.get (if (m.idx+1) == A.length m.images
                       then 0
                       else m.idx + 1
                     ) m.images)
  )



type Action = Next
            | Prev
            | None



update : Action -> Model -> Model
update a m =
  case a of
    None -> m
    Next -> { m | idx <- if m.idx+1 > A.length m.images - 1
                                    then 0
                                    else m.idx + 1
            }
    Prev -> { m | idx <- if m.idx-1 < 0
                                    then A.length m.images - 1
                                    else m.idx - 1
            }


view : Model -> Html
view m =
  let cur = current m
  in div [class "app"]
       [ div [class "slideshow"]
            [ img [class "slide active" , src (fst cur), key (fst cur) ] []
            -- , img [class "slide" , src (snd cur), key (snd cur) ] []
            , img [class "slide", src (snd cur)] []
            ]
       , button [ onClick (Signal.send actionChannel Prev)] [ text "<"]
       , button [ onClick (Signal.send actionChannel Next)] [ text ">"]
       ]
    

model : Signal Model
model = Signal.foldp update init action

actionChannel : Signal.Channel Action
actionChannel = Signal.channel None

action : Signal Action
action =
  Signal.merge
    (Signal.map (\t -> Next) (Time.every (5*Time.second)))
    (Signal.subscribe actionChannel)




main : Signal Html
main =  Signal.map view model
