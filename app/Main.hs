module Main where

import Control.Monad
import Data.Time

--data GenerateEventsStruct = GenerateEvents {
--    subjectId :: Int,
--    eventDuration :: Int,
--    includeWeekend :: Bool,
--    eventGenerationFlow :: Int,
--    dateFrom :: Maybe DateTime,
--    dateTo :: Maybe DateTime,
--    hourFrom :: Maybe TimeOfDay,
--    hourTo :: Maybe TimeOfDay,
--    selectedDays :: [Maybe DateTime]
--
--} deriving Show;

makeUTCTimeValid :: Day -> TimeOfDay -> UTCTime
makeUTCTimeValid d tod = UTCTime { utctDay = d, utctDayTime = timeOfDayToTime tod}

filterWeekDays :: Bool -> [Day] -> [Day]
filterWeekDays True days = days
filterWeekDays _ days = filter (\day -> let dof = dayOfWeek day in dof `elem` [Saturday, Sunday]) days

f = do
    dayFrom <- fromGregorianValid 2010 3 4
    dayTo <- fromGregorianValid 2010 3 10

    let includeWeekend = False;

    return $ map getDatePeriod (filterWeekDays includeWeekend [dayFrom .. dayTo])

getDatePeriod :: Day -> Maybe [(UTCTime, UTCTime)]
getDatePeriod day = do
    hourFrom <- makeTimeOfDayValid 10 30 00
    hourTo <- makeTimeOfDayValid 12 00 00

    let eventDuration = 1800 :: NominalDiffTime
    let eventGap = 0 :: NominalDiffTime

    return $ createEventsPeriods (makeUTCTimeValid day hourFrom) (makeUTCTimeValid day hourTo) eventDuration eventGap

createEventsPeriods :: UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> [(UTCTime, UTCTime)]
createEventsPeriods utcFrom utcTo eventDuration eventGap
    | utcFrom >= utcTo = []
    | otherwise =
        [(utcFrom, addUTCTime (eventDuration - 1) utcFrom)] ++
        createEventsPeriods (addUTCTime (eventDuration + eventGap) utcFrom) utcTo eventDuration eventGap

main = print $ f