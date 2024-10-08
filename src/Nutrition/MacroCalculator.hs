{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Nutrition.MacroCalculator where

import System.Console.Haskeline (InputT, defaultSettings, runInputT, outputStrLn, getInputLine)
import Utils (maybeCase, isDouble)

data Gender = Male | Female
    deriving (Show, Eq)

parseGender :: String -> Gender
parseGender "male" = Male
parseGender "female" = Female
parseGender _ = error "Incorrect gender please enter male or female."

-- The Mifflin St Jeor equation
calcBMR :: Gender -> Double -> Double -> Double -> Double
calcBMR gender age height weight = 
    case gender of
        Male -> bmr' + 5
        Female -> bmr' - 161
    where 
        bmr' = (10 * weight) + (6.25 * height) - (5 * age)

macroCalculator :: IO ()
macroCalculator = runInputT defaultSettings loop 
    where
        getGender :: InputT IO Gender
        getGender = do
            genderStr <- getInputLine "Gender (male/female): "
            maybeCase genderStr getGender (\g ->
                if g `elem` ["male", "Male", "female", "Female"]
                then return . parseGender $ g
                else outputStrLn "Incorrect entry." >> getGender)

        getAge :: InputT IO Double
        getAge = do
            input <- getInputLine "Age: "
            maybeCase input getAge (\ageStr ->
                if isDouble ageStr 
                then let age = read ageStr :: Double 
                      in if age > 0 && age <= 150
                         then return age
                         else outputStrLn "Incorrect entry." >> getAge
                else outputStrLn "Incorrect entry." >> getAge)

        getHeight :: InputT IO Double
        getHeight = do
            input <- getInputLine "Height (cm): "
            maybeCase input getHeight (\heightStr ->
                if isDouble heightStr 
                then let height = read heightStr :: Double 
                         -- 280 is based on the heights of the tallest people
                         -- who ever lived.
                      in if height > 0 && height <= 280
                         then return height
                         else outputStrLn "Incorrect entry." >> getHeight
                else outputStrLn "Incorrect entry." >> getHeight)

        getWeight :: InputT IO Double
        getWeight = do
            input <- getInputLine "Weight (kg): "
            maybeCase input getWeight (\weightStr ->
                if isDouble weightStr 
                then let weight = read weightStr :: Double                          
                      in if weight > 0 && weight <= 670
                         then return weight
                         else outputStrLn "Incorrect entry." >> getWeight
                else outputStrLn "Incorrect entry." >> getWeight)
        
        getActivityLevel :: InputT IO Double
        getActivityLevel = do
            outputStrLn "Activity Level Chart"
            outputStrLn "1.2   : little or no exercise, desk job"
            outputStrLn "1.375 : light exercise/sports 1-3 days/week"
            outputStrLn "1.55  : moderate exercise/sports 6-7 days/week"
            outputStrLn "1.725 : hard exercise every day, or exercising 2 xs/day"
            outputStrLn "1.9   : hard exercise 2 or more times per day, or training for marathon, or triathlon, etc."
            input <- getInputLine "Activity Level: "
            maybeCase input getActivityLevel (\levelStr ->
                if isDouble levelStr 
                then let level = read levelStr :: Double                          
                      in if level >= 1.2 && level <= 1.9
                         then return level
                         else outputStrLn "Incorrect entry." >> getActivityLevel
                else outputStrLn "Incorrect entry." >> getActivityLevel)
        
        getCutCals :: Double -> InputT IO Double
        getCutCals tdee = do        
            input <- getInputLine $ "Calories to cut from TDEE("++(show tdee)++"): "
            maybeCase input (getCutCals tdee) (\cutCalsStr ->
                if isDouble cutCalsStr 
                then let cutCals = read cutCalsStr :: Double                          
                      in if cutCals >= 0
                         then return cutCals
                         else outputStrLn "Incorrect entry." >> (getCutCals tdee)
                else outputStrLn "Incorrect entry." >> (getCutCals tdee))
        
        getPercent :: String -> Double -> InputT IO Double
        getPercent prompt left = do        
            input <- getInputLine $ prompt++" percentage ("++show left++" left) "
            maybeCase input (getPercent prompt left) (\percentStr ->
                if isDouble percentStr 
                then let percent = read percentStr :: Double                          
                      in if percent >= 0 && percent <= 100
                         then return percent
                         else outputStrLn "Incorrect entry." >> getPercent prompt left
                else outputStrLn "Incorrect entry." >> getPercent prompt left)

        getMacros :: Double -> InputT IO (Double, Double, Double, Double, Double, Double)
        getMacros cutTdee = do
            outputStrLn "Macro Breakdown Chart"
            outputStrLn "25-55 : Carbs"
            outputStrLn "25-50 : Protein"
            outputStrLn "20-35 : Fat"
            let percentTotal = 100
            carbPercent <- getPercent "Carbs" percentTotal
            proteinPercent <- getPercent "Protein" (percentTotal - carbPercent)
            fatPercent <- getPercent "Fat" (percentTotal - carbPercent - proteinPercent)
            let carbs = calcCarbGrams cutTdee carbPercent
            let protein = calcProteinGrams cutTdee proteinPercent
            let fat = calcFatGrams cutTdee fatPercent
            return (carbs, carbPercent, protein, proteinPercent, fat, fatPercent)

        loop :: InputT IO ()
        loop = do        
            gender <- getGender
            age <- getAge
            height <- getHeight
            weight <- getWeight
            outputStrLn "Calculating BMR..."
            let bmr = calcBMR gender age height weight                        
            activityLevel <- getActivityLevel
            outputStrLn "Calculating TDEE..."
            let tdee = bmr * activityLevel
            cutCals <- getCutCals tdee
            outputStrLn "Calculating daily calories..."
            let cutTdee = tdee - cutCals                
            (carbs, carbPercent, protein, proteinPercent, fat, fatPercent) <- getMacros cutTdee
            outputStrLn ""
            outputStrLn $ "Athlete's BMR: "++(show bmr)
            outputStrLn $ "Athlete's TDEE: "++(show tdee)
            outputStrLn $ "Athlete's calories: "++(show cutTdee)
            outputStrLn ""
            outputStrLn "Athlete's Marcos: "            
            outputStrLn $ "Carbs   ("++show carbPercent++"%)" ++ ": " ++ show carbs++"g"
            outputStrLn $ "Protein ("++show proteinPercent++"%)"++": "++show protein++"g"
            outputStrLn $ "Fat     ("++show fatPercent++"%)"++": "++show fat++"g"

calcCarbGrams :: Double -> Double -> Double
calcCarbGrams tdee percent = tdee * (percent/100) / 4

calcProteinGrams :: Double -> Double -> Double
calcProteinGrams tdee percent = tdee * (percent/100) / 4

calcFatGrams :: Double -> Double -> Double
calcFatGrams tdee percent = tdee * (percent/100) / 9