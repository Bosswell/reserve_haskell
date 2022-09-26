module Main where

import Control.Monad
import Data.Time

data EventGenerationFlow = Select [Day] | Period Day Day deriving Show

data GenerateEventsStruct = GenerateEvents {
    subjectId :: Int,
    eventDuration :: Int,
    includeWeekend :: Bool,
    eventGenerationFlow :: EventGenerationFlow,
    hourFrom :: TimeOfDay,
    hourTo :: TimeOfDay,
} deriving Show;

makeUTCTimeValid :: Day -> TimeOfDay -> UTCTime
makeUTCTimeValid d tod = UTCTime { utctDay = d, utctDayTime = timeOfDayToTime tod}

filterWeekend :: Bool -> [Day] -> [Day]
filterWeekend True days = days
filterWeekend _ days = filter (\day -> let dof = dayOfWeek day in dof `elem` [Saturday, Sunday]) days

normalizeMinutes :: Int -> Int
normalizeMinutes minutes
                | minutes `mod` 5 == 0 = minutes
                | otherwise = ((minutes `div` 5) + 1) * 5

f = do
    dayFrom <- fromGregorianValid 2010 3 4
    dayTo <- fromGregorianValid 2010 3 10

    let dto = GenerateEvents {
        subjectId = 1,
        eventDuration = 30 * 60,
        includeWeekend = False,
        eventGenerationFlow = Period dayFrom dayTo
--        hourFrom = makeTimeOfDayValid 10 30 00,
--        hourTo = makeTimeOfDayValid 11 50 00
    }

    return $ map getDatePeriod (filterWeekend True [dayFrom .. dayTo])

--createEvent :: Int -> (UTCTime, UTCTime)
--createEvent subjectId

getDatePeriod :: Day -> Maybe [(UTCTime, UTCTime)]
getDatePeriod day = do
    hourFrom <- makeTimeOfDayValid 10 30 00
    hourTo <- makeTimeOfDayValid 11 50 00

    let eventDuration = 1800 :: NominalDiffTime
    let eventGap = 0 :: NominalDiffTime

    return $ createEventsPeriods (makeUTCTimeValid day hourFrom) (makeUTCTimeValid day hourTo) eventDuration eventGap

createEventsPeriods :: UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> [(UTCTime, UTCTime)]
createEventsPeriods utcFrom utcTo eventDuration eventGap
    | utcFrom >= utcTo = []
    | utcTo < utcToInPeriod = []
    | otherwise =
        [(utcFrom, utcToInPeriod)] ++
        createEventsPeriods (addUTCTime (eventDuration + eventGap) utcFrom) utcTo eventDuration eventGap
    where utcToInPeriod = addUTCTime (eventDuration - 1) utcFrom

main = print $ f