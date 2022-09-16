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

getDatePeriod :: Maybe [(UTCTime, UTCTime)]
getDatePeriod = do
    let eventDuration = 1800 :: NominalDiffTime
    let eventGap = 0 :: NominalDiffTime

    dayFrom <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04"
    hourFrom <- makeTimeOfDayValid 10 30 00

    dayTo <- parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04"
    hourTo <- makeTimeOfDayValid 18 00 00

    return $ createEventsPeriods (makeUTCTimeValid dayFrom hourFrom) (makeUTCTimeValid dayTo hourTo) eventDuration eventGap

createEventsPeriods :: UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> [(UTCTime, UTCTime)]
createEventsPeriods utcFrom utcTo eventDuration eventGap
    | utcFrom >= utcTo = []
    | otherwise =
        [(utcFrom, addUTCTime (eventDuration - 1) utcFrom)] ++
        createEventsPeriods (addUTCTime (eventDuration + eventGap) utcFrom) utcTo eventDuration eventGap

main = print $ getDatePeriod

