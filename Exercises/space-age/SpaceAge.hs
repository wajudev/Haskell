module SpaceAge (
  main
) where

    import System.Environment (getArgs)
    import Text.Read (readMaybe)
    import Data.Maybe (fromJust)

    data Planet = Mercury
                | Venus
                | Earth
                | Mars
                | Jupiter
                | Saturn
                | Uranus
                | Neptune
      deriving (Show, Read)

    orbitalPeriod :: Planet -> Float
    orbitalPeriod planet =
        let
            earthYear = 31557600
        in 
            case planet of 
                Mercury -> 0.24084670 * earthYear
                Venus   -> 1.61519726 * earthYear -- BUG
                Earth   -> 1.00000000 * earthYear
                Mars    -> 1.88081580 * earthYear
                Jupiter -> 11.8626150 * earthYear
                Saturn  -> 29.4474980 * earthYear
                Uranus  -> 84.0168460 * earthYear
                Neptune -> 164.791320 * earthYear

    ageOn :: Planet -> Float -> Float
    ageOn planet age = age / (orbitalPeriod planet)

    readPlanet :: IO Planet
    readPlanet = do
        putStrLn "Enter a planet:"
        planetStr <- getLine
        case readMaybe planetStr of
             Just planet -> return planet
             Nothing -> do
                 putStrLn "Invalid planet. Please enter a valid planet."
                 readPlanet

    readAge :: IO Float
    readAge = do
        putStrLn "Enter your age:"
        ageStr <- getLine
        case readMaybe ageStr of
            Just age -> return age
            Nothing -> do
                putStrLn "Invalid age. Please enter a valid age."
                readAge

    main :: IO ()
    main = do
       planet <- readPlanet
       age <- readAge
       let ageOnPlanet = ageOn planet age
       putStrLn $ "Your age on " ++ show planet ++ " is " ++ show ageOnPlanet ++ " years."