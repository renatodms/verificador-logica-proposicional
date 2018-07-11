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

-- | Data describing the state of the asteroids interface.
data InterfaceComparadorLogicaProposicional = Interface
  { 
    leftValue :: String,
    rightValue :: String,
    message :: String,
    verifyButtonCoordinate :: (Float, Float),
    verifyButtonDimension :: (Float, Float),
    labelDimension :: (Float, Float),
    leftCoordinate :: (Float, Float),
    rightCoordinate :: (Float, Float),
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
initialState = Interface
  { 
    leftValue = "",
    rightValue = "",
    message = "",
    verifyButtonCoordinate = (-50, 0),
    verifyButtonDimension = (100, 50),
    labelDimension = (500, 50),
    leftCoordinate = (-275, 200),
    rightCoordinate = (275, 200),    
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
checkClick interface = if not clicked' then interface {leftValueCoordinate = leftValueCoordinate', rightValueCoordinate = rightValueCoordinate'} else interface { leftColor = leftColor', rightColor = rightColor', selectedLabel = selectedLabel', message = message'}
  where
    clicked' = clicked interface
    click = clickedCoordinate interface
    dimension = labelDimension interface    
    max = maxSize interface

    leftOffset = leftSize interface - max
    (xl, yl) = leftValueCoordinate interface

    rightOffset = rightSize interface - max
    (xr, yr) = rightValueCoordinate interface

    leftValueCoordinate' = ( if leftOffset > 0 then -leftOffset * 11 + (leftXmin interface) else (leftXmin interface) , yl )
    rightValueCoordinate' =  ( if rightOffset > 0 then -rightOffset * 11 + (rightXmin interface) else (rightXmin interface) , yr )

    leftColor' = if clickedLeft then (selectedColor interface) else (deselectedColor interface)
    rightColor' = if clickedRight then (selectedColor interface) else (deselectedColor interface)

    selectedLabel' = if clickedLeft then 1 else if clickedRight then 2 else 0

    message' = if clickedVerify then "teste" else message interface

    clickedVerify = checkAreaClicked click (verifyButtonCoordinate interface) (verifyButtonDimension interface)

    clickedLeft = checkAreaClicked click (leftCoordinate interface) dimension
    clickedRight = checkAreaClicked click (rightCoordinate interface) dimension

checkAreaClicked :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
checkAreaClicked (x, y) (lX, lY) (dH, dV) = collided
  where
    collided = collidedX && collidedY
    collidedX = ( lX - dH / 2.0 < x ) && ( x < lX + dH / 2.0 )
    collidedY = ( lY - dV / 2.0 < y ) && ( y < lY + dV / 2.0 )

render :: InterfaceComparadorLogicaProposicional -> Picture
render interface = pictures [renderLabelRight, renderValueRight, renderDrawProtection2, renderLabelLeft, renderValueLeft, renderEqual, renderDrawProtection1, renderLegend, renderButton, renderResult]
  where        
    renderLabelLeft = drawLabelLeft
    renderLabelRight = drawLabelRight
    renderValueLeft = drawValueLeft (leftValue interface)
    renderValueRight = drawValueRight (rightValue interface)
    renderEqual = drawEqual "=="
    renderDrawProtection1 = drawProtectionArea1
    renderDrawProtection2 = drawProtectionArea2
    renderLegend = drawLegend
    renderButton = drawVerifyButton
    renderResult = drawResult

    drawLabelLeft :: Picture
    drawLabelLeft = uncurry translate (leftCoordinate interface) $ color (leftColor interface) $ (uncurry rectangleWire (labelDimension interface))

    drawLabelRight :: Picture
    drawLabelRight = uncurry translate (rightCoordinate interface) $ color (rightColor interface) $ (uncurry rectangleWire (labelDimension interface))    

    drawValueLeft :: String -> Picture
    drawValueLeft value = uncurry translate (leftValueCoordinate interface) $ scale 0.15 0.15 $ color black $ Text value

    drawValueRight :: String -> Picture
    drawValueRight value = uncurry translate (rightValueCoordinate interface) $ scale 0.15 0.15 $ color black $ Text value

    drawEqual :: String -> Picture
    drawEqual value = uncurry translate (-15, 190) $ scale 0.15 0.15 $ color black $ Text value

    drawProtectionArea1 :: Picture
    drawProtectionArea1 = uncurry translate (-551, 205)  $ color white $ rectangleSolid 50 50

    drawProtectionArea2 :: Picture
    drawProtectionArea2 = uncurry translate (-260, 205)  $ color white $ rectangleSolid 568 50
    
    drawResult :: Picture
    drawResult = uncurry translate (50, -10) $ scale 0.25 0.25 $ color (dark red) $ Text (message interface)

    drawVerifyButton :: Picture
    drawVerifyButton = pictures [buttonBase, buttonText]
      where
        (xB, yB) = (verifyButtonCoordinate interface)
        (xD, yD) = (verifyButtonDimension interface)
        textPosition = (xB - xD / 2.5, yB - yD / 5.0)
        buttonBase = uncurry translate (verifyButtonCoordinate interface) $ color cyan $ (uncurry rectangleSolid (verifyButtonDimension interface))
        buttonText = uncurry translate textPosition $ scale 0.15 0.15 $ color black $ Text "Verificar!"
    
    drawLegend :: Picture
    drawLegend = pictures [legend1, legend2, legend3, legend4, legend5, legend6]
      where
        legend1 = uncurry translate (-500, 100) $ scale 0.15 0.15 $ color black $ Text "'T' para Const True"
        legend2 = uncurry translate (-500, 50) $ scale 0.15 0.15 $ color black $ Text "'F' para Const False"
        legend3 = uncurry translate (-500, 0) $ scale 0.15 0.15 $ color black $ Text "'&' para Conj"
        legend4 = uncurry translate (-500, -50) $ scale 0.15 0.15 $ color black $ Text "'|' para Disj"
        legend5 = uncurry translate (-500, -100) $ scale 0.15 0.15 $ color black $ Text "'>' para Imp"                
        legend6 = uncurry translate (-500, -150) $ scale 0.15 0.15 $ color black $ Text "'!' para Not" 

handleKeys :: Event -> InterfaceComparadorLogicaProposicional -> InterfaceComparadorLogicaProposicional
handleKeys (EventKey (SpecialKey KeyDelete) Down _ _) interface = interface { leftValue = leftValue', rightValue = rightValue', leftSize = leftSize', rightSize = rightSize' }
  where
    leftValue' = if (selectedLabel interface) == 1 then removeLastValue (leftValue interface) else (leftValue interface)
    rightValue' = if (selectedLabel interface) == 2 then removeLastValue (rightValue interface) else (rightValue interface)
    newLeft = (if (selectedLabel interface) == 1 then (leftSize interface - 1) else (leftSize interface))
    newRight = (if (selectedLabel interface) == 2 then (rightSize interface - 1) else (rightSize interface))

    leftSize' = max newLeft 0
    rightSize' = max newRight 0
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) interface = interface { leftValue = leftValue', rightValue = rightValue', leftSize = leftSize', rightSize = rightSize'  }
  where
    leftValue' = if (selectedLabel interface) == 1 then (leftValue  interface) ++ " " else (leftValue interface)
    rightValue' = if (selectedLabel interface) == 2 then (rightValue  interface) ++ " " else (rightValue interface)
    leftSize' = if (selectedLabel interface) == 1 then (leftSize interface + 1) else (leftSize interface)
    rightSize' = if (selectedLabel interface) == 2 then (rightSize interface + 1) else (rightSize interface)
handleKeys (EventKey (Char charText) Down _ _) interface = interface { leftValue = leftValue', rightValue = rightValue', leftSize = leftSize', rightSize = rightSize'  }
  where
    leftValue' = if (selectedLabel interface) == 1 then (leftValue interface) ++ [charText] else (leftValue interface)
    rightValue' = if (selectedLabel interface) == 2 then (rightValue interface) ++ [charText] else (rightValue interface)
    leftSize' = if (selectedLabel interface) == 1 then (leftSize interface + 1) else (leftSize interface)
    rightSize' = if (selectedLabel interface) == 2 then (rightSize interface + 1) else (rightSize interface)
handleKeys (EventKey (MouseButton LeftButton) Down _ (x, y)) interface = interface { clicked = True, clickedCoordinate = (x, y) }
handleKeys (EventKey (MouseButton LeftButton) Up _ _) interface = interface { clicked = False}
handleKeys _ interface = interface

removeLastValue :: String -> String
removeLastValue [] = []
removeLastValue xs = init xs

fps :: Int
fps = 60

main :: IO ()
main = play window background fps initialState render handleKeys update

update :: Float -> InterfaceComparadorLogicaProposicional -> InterfaceComparadorLogicaProposicional
update seconds interface = checkClick interface

