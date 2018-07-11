module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Data.Vector
import Parser

width, height, offsetWidth, offsetHeight :: Int
width = 1100
height = 650
offsetWidth = 125
offsetHeight = 15

window :: Display
window = InWindow "Asteroids" (width, height) (offsetWidth, offsetHeight)

background :: Color
background = white

-- | Data describing the state of the asteroids game.
data InterfaceComparadorLogicaProposicional = Game
  { 
    leftValue :: String,
    rightValue :: String,
    labelDimension :: (Float, Float),
    leftCoordinatePosition :: (Float, Float),
    rightCoordinatePosition :: (Float, Float),
    leftValueCoordinate :: (Float, Float),
    rightValueCoordinate :: (Float, Float),
    leftSize :: Float,
    rightSize :: Float,
    leftXmin :: Float,
    rightXmin :: Float,
    maxSize :: Float,
    clicked :: Bool,
    clickedCoordinate :: (Float, Float),
    leftColor :: Color,
    rightColor :: Color,
    selectedLabel :: Int,
    selectedColor :: Color,
    deselectedColor :: Color
  } deriving Show

initialState :: InterfaceComparadorLogicaProposicional
initialState = Game
  { 
    leftValue = "",
    rightValue = "",
    labelDimension = (500, 50),
    leftCoordinatePosition = (-275, 200),
    rightCoordinatePosition = (275, 200),    
    leftValueCoordinate = (-510, 190),    
    rightValueCoordinate = (40, 190),
    leftSize = 0,
    rightSize = 0,
    leftXmin = -510,
    rightXmin = 40,
    maxSize = 35,
    clicked = False,
    clickedCoordinate = (0, 0),
    leftColor = (light (makeColorI 160 160 160 255)),
    rightColor = (light (makeColorI 160 160 160 255)),
    selectedLabel = 0,
    selectedColor = black,
    deselectedColor = (light (makeColorI 160 160 160 255))
  }

checkClick :: InterfaceComparadorLogicaProposicional -> InterfaceComparadorLogicaProposicional
checkClick game = if not clicked' then game {leftValueCoordinate = leftValueCoordinate', rightValueCoordinate = rightValueCoordinate'} else game { leftColor = leftColor', rightColor = rightColor', selectedLabel = selectedLabel'}
  where
    clicked' = clicked game
    click = clickedCoordinate game
    dimension = labelDimension game    
    max = maxSize game

    leftOffset = leftSize game - max
    (xl, yl) = leftValueCoordinate game

    rightOffset = rightSize game - max
    (xr, yr) = rightValueCoordinate game

    leftValueCoordinate' = ( if leftOffset > 0 then -leftOffset * 11 + (leftXmin game) else (leftXmin game) , yl )
    rightValueCoordinate' =  ( if rightOffset > 0 then -rightOffset * 11 + (rightXmin game) else (rightXmin game) , yr )

    leftColor' = if clickedLeft then (selectedColor game) else (deselectedColor game)
    rightColor' = if clickedRight then (selectedColor game) else (deselectedColor game)

    selectedLabel' = if clickedLeft then 1 else if clickedRight then 2 else 0

    clickedLeft = checkAreaClicked click (leftCoordinatePosition game) dimension
    clickedRight = checkAreaClicked click (rightCoordinatePosition game) dimension

checkAreaClicked :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
checkAreaClicked (x, y) (lX, lY) (dH, dV) = collided
  where
    collided = collidedX && collidedY
    collidedX = ( lX - dH / 2.0 < x ) && ( x < lX + dH / 2.0 )
    collidedY = ( lY - dV / 2.0 < y ) && ( y < lY + dV / 2.0 )

render :: InterfaceComparadorLogicaProposicional -> Picture
render game = pictures [renderLabelRight, renderValueRight, renderDrawProtection2, renderLabelLeft, renderValueLeft, renderEqual, renderDrawProtection1, renderLegend]
  where        
    renderLabelLeft = drawLabelLeft
    renderLabelRight = drawLabelRight
    renderValueLeft = drawValueLeft (leftValue game)
    renderValueRight = drawValueRight (rightValue game)
    renderEqual = drawEqual "=="
    renderDrawProtection1 = drawProtectionArea1
    renderDrawProtection2 = drawProtectionArea2
    renderLegend = drawLegend

    drawLabelLeft :: Picture
    drawLabelLeft = uncurry translate (leftCoordinatePosition game) $ color (leftColor game) $ (uncurry rectangleWire (labelDimension game))

    drawLabelRight :: Picture
    drawLabelRight = uncurry translate (rightCoordinatePosition game) $ color (rightColor game) $ (uncurry rectangleWire (labelDimension game))    

    drawValueLeft :: String -> Picture
    drawValueLeft value = uncurry translate (leftValueCoordinate game) $ scale 0.15 0.15 $ color black $ Text value

    drawValueRight :: String -> Picture
    drawValueRight value = uncurry translate (rightValueCoordinate game) $ scale 0.15 0.15 $ color black $ Text value

    drawEqual :: String -> Picture
    drawEqual value = uncurry translate (-15, 190) $ scale 0.15 0.15 $ color black $ Text value

    drawProtectionArea1 :: Picture
    drawProtectionArea1 = uncurry translate (-551, 205)  $ color white $ rectangleSolid 50 50

    drawProtectionArea2 :: Picture
    drawProtectionArea2 = uncurry translate (-260, 205)  $ color white $ rectangleSolid 568 50 
    
    drawLegend :: Picture
    drawLegend = pictures [legend1, legend2, legend3, legend4, legend5, legend6]
      where
        legend1 = uncurry translate (-500, 100) $ scale 0.2 0.2 $ color black $ Text "'T' para Const True"
        legend2 = uncurry translate (-500, 50) $ scale 0.2 0.2 $ color black $ Text "'F' para Const False"
        legend3 = uncurry translate (-500, 0) $ scale 0.2 0.2 $ color black $ Text "'&' para Conj"
        legend4 = uncurry translate (-500, -50) $ scale 0.2 0.2 $ color black $ Text "'|' para Disj"
        legend5 = uncurry translate (-500, -100) $ scale 0.2 0.2 $ color black $ Text "'>' para Imp"                
        legend6 = uncurry translate (-500, -150) $ scale 0.2 0.2 $ color black $ Text "'!' para Not" 

handleKeys :: Event -> InterfaceComparadorLogicaProposicional -> InterfaceComparadorLogicaProposicional
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) game = game { leftValue = leftValue', rightValue = rightValue', leftSize = leftSize', rightSize = rightSize' }
  where
    leftValue' = if (selectedLabel game) == 1 then removeLastValue (leftValue game) else (leftValue game)
    rightValue' = if (selectedLabel game) == 2 then removeLastValue (rightValue game) else (rightValue game)
    newLeft = (if (selectedLabel game) == 1 then (leftSize game - 1) else (leftSize game))
    newRight = (if (selectedLabel game) == 2 then (rightSize game - 1) else (rightSize game))

    leftSize' = max newLeft 0
    rightSize' = max newRight 0
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = game { leftValue = leftValue', rightValue = rightValue', leftSize = leftSize', rightSize = rightSize'  }
  where
    leftValue' = if (selectedLabel game) == 1 then (leftValue  game) ++ " " else (leftValue game)
    rightValue' = if (selectedLabel game) == 2 then (rightValue  game) ++ " " else (rightValue game)
    leftSize' = if (selectedLabel game) == 1 then (leftSize game + 1) else (leftSize game)
    rightSize' = if (selectedLabel game) == 2 then (rightSize game + 1) else (rightSize game)
handleKeys (EventKey (Char charText) Down _ _) game = game { leftValue = leftValue', rightValue = rightValue', leftSize = leftSize', rightSize = rightSize'  }
  where
    leftValue' = if (selectedLabel game) == 1 then (leftValue game) ++ [charText] else (leftValue game)
    rightValue' = if (selectedLabel game) == 2 then (rightValue game) ++ [charText] else (rightValue game)
    leftSize' = if (selectedLabel game) == 1 then (leftSize game + 1) else (leftSize game)
    rightSize' = if (selectedLabel game) == 2 then (rightSize game + 1) else (rightSize game)
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) game = game { clicked = True, clickedCoordinate = (x, y) }
handleKeys (EventKey (MouseButton LeftButton) Up _ _) game = game { clicked = False}
handleKeys _ game = game

removeLastValue :: String -> String
removeLastValue [] = []
removeLastValue xs = init xs

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> InterfaceComparadorLogicaProposicional -> InterfaceComparadorLogicaProposicional
update seconds game = checkClick game

