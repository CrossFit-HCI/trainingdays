{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Nutrition.MacroCalculator where
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

    calcHBBMR :: Gender -> Double -> Double -> Double -> Double
    calcHBBMR gender age height weight = 
        case gender of
            Male -> undefined
            Female -> (9.5634 * weight) + (1.8496 * height) - (4.6756 * age) + 66.473

    macroCalculator :: IO ()
    macroCalculator = loop 
        where
            loop :: IO ()
            loop = do
                putStr "Gender: "
                genderStr <- getLine
                let gender = parseGender genderStr
                putStr "Age: "
                age <- getLine >>= \s -> return (read s :: Double)
                putStr "Height in cm: "
                height <- getLine >>= \s -> return (read s :: Double)                
                putStr "Weight in kg: "
                weight <- getLine >>= \s -> return (read s :: Double)
                let bmr = calcBMR gender age height weight
                putStrLn "Activity Level Chart"
                putStrLn "1.2   : little or no exercise, desk job"
                putStrLn "1.375 : light exercise/sports 1-3 days/week"
                putStrLn "1.55  : moderate exercise/sports 6-7 days/week"
                putStrLn "1.725 : hard exercise every day, or exercising 2 xs/day"
                putStrLn "1.9   : hard exercise 2 or more times per day, or training for marathon, or triathlon, etc."
                putStr "activity level: "
                activityLevel <- getLine >>= \s -> return (read s :: Double)
                let tdee = bmr * activityLevel
                putStr "calories to cut from tdee: "
                cutCals <- getLine >>= \s -> return (read s :: Double)
                let cutTdee = tdee - cutCals                
                putStrLn $ "Macro Breakdown Chart"
                putStrLn $ "25-55 : Carbs"
                putStrLn $ "25-50 : Protein"
                putStrLn $ "20-35 : Fat"
                let precentTotal = 100
                putStr $ "carbs percentage ("++show precentTotal++" left): "
                carbPercent <- getLine >>= \s -> return (read s :: Double)
                putStr $ "proten percentage ("++show (precentTotal - carbPercent)++" left): "
                proteinPercent <- getLine >>= \s -> return (read s :: Double)
                putStr $ "fat percentage ("++show (precentTotal - carbPercent - proteinPercent)++" left): "
                fatPercent <- getLine >>= \s -> return (read s :: Double)
                putStrLn ""
                putStrLn $ "Athlete's BMR: "++(show bmr)
                putStrLn $ "Athlete's TDEE: "++(show tdee)
                putStrLn $ "Athlete's calories: "++(show cutTdee)
                putStrLn "Athlete's Marcos: "
                let carbs = calcCarbGrams cutTdee carbPercent
                let protein = calcProteinGrams cutTdee proteinPercent
                let fat = calcFatGrams cutTdee fatPercent
                putStrLn $ "Carbs   ("++show carbPercent++"%)" ++ ": " ++ show carbs++"g"
                putStrLn $ "Protein ("++show proteinPercent++"%)"++": "++show protein++"g"
                putStrLn $ "Fat     ("++show fatPercent++"%)"++": "++show fat++"g"

    calcCarbGrams :: Double -> Double -> Double
    calcCarbGrams tdee percent = tdee * (percent/100) / 4

    calcProteinGrams :: Double -> Double -> Double
    calcProteinGrams tdee percent = tdee * (percent/100) / 4

    calcFatGrams :: Double -> Double -> Double
    calcFatGrams tdee percent = tdee * (percent/100) / 9