import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple as StartApp
import Text exposing (..)
import List 
import Basics exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Color exposing (..)
import Keyboard
import Time exposing (..)
import Touch exposing (..)

type alias Keys = { x:Int, y:Int }


areaWidth = 500 -- change the width of the play area 
areaHeight = 300 -- change the height of the play area

--Hackathon API Code: Change things here!!

--use this to change the player graphics!
player = group [  circle 10 |> filled black, 
                  circle 9 |> filled yellow
               ]

--Add new lines to this list to add new potholes or change the current ones
obsList = [
           (100,75)
          ,(90,-50)
          ,(110,10)
          ,(-100,-50)
          ,(-50,-40)
          ,(-90,30)
          ,(60,60)
          ]

--Change size of trap
--(1 is normal, more than 1 is bigger)
trapSize = 1

--Change background colour of the play area
backgroundColour = darkGreen

initX = -150 --Change the starting x position of the player
initY = 0 -- Change the starting y-position of the player

questionTxt = "(5+10)*5 = ?" --change the question 

--What are the possible answers?
answerATxt = "75"
answerBTxt = "15"
answerCTxt = "20"
answerDTxt = "17"

--Which answer is correct?
answerA = Correct
answerB = Wrong
answerC = Wrong
answerD = Wrong
               

type Answer = Correct | Wrong

--Change the background shapes /colors of the quiz options
answerBG= group[ rect 50 50           -- first option
                      |> filled white
                      |> move (200,-35)
                      , rect 50 50    --second option
                      |> filled white 
                      |> move (200,-100)
                      , rect 50 50    --third option
                      |> filled white 
                      |> move (200,100)
                      , rect 50 50    --fourth option
                      |> filled white
                      |> move (200,35)
               ]   
    















--Start time 


{-| Read more about StartApp and how this works at:

    https://github.com/evancz/start-app

The rough idea is that we just specify a model, a way to view it,
and a way to update it. That's all there is to it!
-}


main : Signal Html
main =
  Signal.map view (Signal.foldp update model input)

input : Signal (Float, Keys, List Touch)
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)
  in
    Signal.sampleOn delta (Signal.map3 triple delta Keyboard.arrows Touch.touches)

triple a b c = (a,b,c)

type alias Model = { x : Float, y : Float, areaW: Float, areaH: Float, onTrap: Bool, obstacleList : List (Float,Float), 
                    visibleList: List Bool, gameEnded: Bool}

model = { x = initX, y = initY , areaW = areaWidth, areaH = areaHeight, onTrap = False, 
          obstacleList = obsList,
          visibleList = List.repeat len False, gameEnded = False}

len = List.length obsList

view model = div [] [fromElement (shape model)]
                  


shape model = collage (round (model.areaW)) (round (model.areaH)) 
                      [ rect (model.areaW) (model.areaH) |> filled black
                      , rect (model.areaW-10) (model.areaH-10) |> filled backgroundColour
                      , question |> move(-185,110)
                      , obstacleDrawer model.obstacleList model.visibleList
                      ,player
                      |> move (model.x,model.y) 
                      ,answerBG]

question = group [rect 102 52 |> filled black, rect 100 50 |> filled green, questionText]
questionText = group [Graphics.Collage.text (Text.bold(Text.fromString questionTxt))]

obstacleDrawer l lvis = group (List.map2 drawObstacle l lvis)
drawObstacle (x,y) vis = (obstacle vis) |> move (x, y)
obstacle vis = group [if vis then pothole else filled black (circle 0)]
obsradius = 700

pothole = group [ (filled black (oval (trapSize*35) (trapSize*16))) |> scale 1.05
                 , (filled darkBrown (oval (trapSize*35) (trapSize*16)))
                 , (filled black (oval (trapSize*34) (trapSize*10))) |> scale 0.98 |> move (0,-(2.7*trapSize))]

--limit a b = --checker 100 75 85 110 50 100
--            if a >= 85 && a <= 110 && b >= 50 && b <= 100 then 85 
--            else if a >= 55 && a <= 80 && b >= -75 && b <= -25 then 55
--            else if a >= 105 && a <= 130 && b >= -15 && b <= 35 then 135
--            else if a >= -115 && a <= -90 && b >= -125 && b <= -75 then -115
--            else if a >= -65 && a <= -40 && b >= -45 && b <= 5 then -65
--           else if a >= -105 && a <= -80 && b >= 45 && b <= 95 then -105
--            else a 
--mapNextTo model (x,y) = if model.x >= ((fst (model.obstacleList)) - 15) && model.x >= ((fst (model.obstacleList)) + 10) && model.y >= ((snd (model.obstacleList)) - 25) && model.y >= ((snd (model.obstacleList)) + 25)
--                    then ((fst (model.obstacleList)) - 15)
--                else model.x
--mapNextTo model (x,y) = if model.x >= (x - 15) && model.x <= (x + 10) && model.y >= (y - 25) && model.y <= (y + 25)
--                    then (x-15) else model.x
--mapNextToList = List.map (mapNextTo model) model.obstacleList
--nextTo model = if model.x == List.map mapNextTo model.obstacleList then True else False

--funcname y= case y of:
            --tell you whether or not model.x is next to the object in the list obstacle list
            --(x::xs) -> funcname xs
            
--updateScore a b = if 
--checker a b xL xR yU yD = if a >= xL && a <= xR && b >= yD && b <= yU then xL 
                          --else a

--Check given coordinate of trap comparison to model character location
onCoordinate : { a | x : number', y : number } -> (number,number) -> Bool

onCoordinate model (x,y) = func model (x,y) <= obsradius                           

trapChecker model = List.map (onCoordinate model) model.obstacleList

visUpdate model = List.map2 (onCoordinateVis model) model.obstacleList model.visibleList
onCoordinateVis model (x,y) visible = if func model (x,y) <= obsradius && visible == False then True 
                                   else if visible then True 
                                   else False

func model (x,y)= ((x-model.x)^2+(y-model.y)^2)
--Return True or false
listChecker : { a | x : number', y : number, obstacleList: List(number,number) } -> Bool
listChecker model = List.foldr (||) (False) (trapChecker model) 
--If empty, no true (no trap contact), thus return false else true
checkTrap model = (listChecker model) 

resetX = -150
resetY = 0

update : (Float, Keys, List Touch) -> Model -> Model
update (dt, keys, touches) model =
  model |> updateModel keys dt touches

updateModel keys dt touches model  = let touch = Maybe.withDefault { x = 0
                                          , y = 0
                                          , id = 69
                                          , x0 = 0
                                          , y0 = 0
                                          , t0 = 0
                                          }  (List.head touches)
                                          
                                     in -- Gets the top of the list.
                                          { model | onTrap = checkTrap model,
                                          visibleList= visUpdate model,
                                          gameEnded = checkAnswer model,
                                          y = if not model.onTrap then model.y + (toFloat keys.y) * dt else resetY,
                                          x = if not model.onTrap then model.x + toFloat keys.x * dt else resetX}


touchMove touch model =
  let x = toFloat touch.x
  in if touch.id == 69 then 0
     else if (x-areaWidth) > (model.x+6) then 1
     else if (x-areaWidth) < (model.x-6) then -1
     else 0



checkAnswer model = if model.x > 190 && model.y > 75 && answerA == Correct then True 
                    else if model.x > 190 && model.y > 0 && model.y < 75 && answerB == Correct then True
                    else if model.x > 190 && model.y > -75 && model.y < 0 && answerC == Correct then True
                    else if model.x > 190 && model.y > -150 && model.y < -75 && answerD == Correct then True
                    else False