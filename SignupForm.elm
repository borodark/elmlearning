-- declares that this is the SignupForm module, which is how
-- other modules will reference this one if they want to import it and reuse its code.


module SignupForm (..) where

-- Elm’s "import" keyword works mostly like "require" in node.js.
-- The “exposing (..)” option specifies that we want to bring the Html module’s contents
-- into this file’s current namespace, so that instead of writing out
-- Html.form and Html.label we can just use "form" and "label" without the "Html." prefix.

import Html exposing (..)


-- This works the same way; we also want to import the entire
-- Html.Events module into the current namespace.

import Html.Events exposing (..)


-- With this import we are only bringing a few specific functions into our
-- namespace, specifically "id", "type'", "for", "value", and "class".

import Html.Attributes exposing (id, type', for, value, class, tabindex)
import StartApp
import Effects
import Http
import Task exposing (Task)
import Json.Decode exposing (succeed)

type ActionType = Validate | SetUsername | SetPassword | UsernameTaken | UsernameAvailable 

type alias Action = {actionType:ActionType, payload:String }    
    
type alias Errors =
    { username:String, password:String, usernameTaken:Bool}

type alias Model  = {counter:Int, username:String, password:String, errors:Errors }

type alias UpdateResult = (Model, Effects.Effects )    
    
view actionDispatcher model =
    form [ id "signup-form" ]
        [ h1 [] [ text "Sensational Signup Form" ]
        , text (toString model.counter)
        , label [ for "username-field" ] [ text "username: " ]
        , input
            [ id "username-field"
            , tabindex 3
            , type' "text"
            , value model.username
            , on "input" targetValue (\str -> Signal.message actionDispatcher { actionType = SetUsername, payload = str })
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.username ]
        , label [ for "password" ] [ text "password: " ]
        , input
            [ id "password-field"
              -- , class "signup-button"
            , tabindex 2
            , type' "password"
            , value model.password
            , on "input" targetValue (\str -> Signal.message actionDispatcher { actionType = SetPassword, payload = str })
            ]
            []
        , div [ class "validation-error" ] [ text model.errors.password ]
        , div
            [ class "signup-button"
            , tabindex 1
            , onClick actionDispatcher { actionType = Validate, payload = "" }
            ]
            [ text "Sign Up!" ]
        , div [ class "validation-error" ]
            [ text (viewUsernameErrors model) ]
        ]


viewUsernameErrors model =
    if model.errors.usernameTaken then
        "That username is taken!"
    else
        model.errors.username

getErrors model =
    { username =
        if model.username == "" then
            "Please enter a username!"
        else
            ""
    , password =
        if model.password == "" || model.password == "password" then
            "Please enter a password!"
        else
            ""
    , usernameTaken = model.errors.usernameTaken
    }

update: Action -> Model -> UpdateResult
    
update action model =
    if action.actionType == Validate then
        ( { model | errors = getErrors model }, Effects.task (neverFailingRequest model) )
    else if action.actionType == SetUsername then
        ( { model | username = action.payload, counter = model.counter + 1 }, Effects.none )
    else if action.actionType == SetPassword then
        ( { model | password = action.payload, counter = model.counter + 1 }, Effects.none )
    else if action.actionType == UsernameTaken then
        ( withUsernameTaken True model, Effects.none )
    else if action.actionType == UsernameAvailable then
        ( withUsernameTaken False model, Effects.none )
    else
        ( model, Effects.none )


neverFailingRequest model =
    Task.onError
        (Http.get (succeed { actionType = UsernameTaken, payload = "" })
            ("https://api.github.com/users/" ++ model.username)
        )
        (\err -> Task.succeed { actionType = UsernameAvailable, payload = "" })


withUsernameTaken isTaken model =
    let
        currentErrors =
            model.errors

        newErrors =
            { currentErrors | usernameTaken = isTaken }
    in
        { model | errors = newErrors }

initialErrors:Errors
             
initialErrors = { username = "", password = "", usernameTaken = False }

initialModel:Model         
initialModel =
    { counter = 0, username = "", password = "", errors = initialErrors }


initials:UpdateResult
initials = (initialModel, Effects.none)
          

-- Take a look at this starting model we’re passing to our view function.
-- Note that in Elm syntax, we use = to separate fields from values
-- instead of : like JavaScript uses for its object literals.


app =
    StartApp.start
        { init = initials -- ( initialModel, Effects.none )
        , update = update
        , view = view
        , inputs = []
        }


main =
    app.html


port tasks : Signal (Task Effects.Never ())
port tasks =
    app.tasks
