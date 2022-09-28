module Main where

import Control.Monad
import Data.Time

data EventGenerationFlow = Select [Day] | Period Day Day deriving Show

data GenerateEventsStruct = GenerateEvents {
    subjectId :: Int,
    eventDuration :: Int,
    includeWeekend :: Bool,
    eventGenerationFlow :: EventGenerationFlow,
    hourFrom :: Maybe TimeOfDay,
    hourTo :: Maybe TimeOfDay
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

createEvent :: Int -> (UTCTime, UTCTime) -> Maybe ()
createEvent a b | eventCanBeCreated == False = Nothing
                | otherwise = Just ()
    where eventCanBeCreated = doesSubjectHasOngoingEventsInPeriod a b

-- TODO Get info from DB
doesSubjectHasOngoingEventsInPeriod :: Int -> (UTCTime, UTCTime) -> Bool
doesSubjectHasOngoingEventsInPeriod _ _ = False

-- TODO Do some SQL insert
insertEvent :: Int -> (UTCTime, UTCTime) -> ()
insertEvent subjectId (utcFrom, utcTo) = ()

getEventsOfTheDay :: TimeOfDay -> TimeOfDay -> Day -> [(UTCTime, UTCTime)]
getEventsOfTheDay hourFrom hourTo day = createEventsPeriods (makeUTCTimeValid day hourFrom) (makeUTCTimeValid day hourTo) eventDuration eventGap
    where eventDuration = 1800
          eventGap = 0

createEventsPeriods :: UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> [(UTCTime, UTCTime)]
createEventsPeriods utcFrom utcTo eventDuration eventGap
    | utcFrom >= utcTo = []
    | utcTo < utcToInPeriod = []
    | otherwise =
        [(utcFrom, utcToInPeriod)] ++
        createEventsPeriods (addUTCTime (eventDuration + eventGap) utcFrom) utcTo eventDuration eventGap
    where utcToInPeriod = addUTCTime (eventDuration - 1) utcFrom

createEvents :: Maybe [Maybe ()]
createEvents = do
    dayFrom <- fromGregorianValid 2010 3 4
    dayTo <- fromGregorianValid 2010 3 10

    hourFrom <- makeTimeOfDayValid 10 30 00
    hourTo <- makeTimeOfDayValid 11 50 00

    let dto = GenerateEvents {
        subjectId = 1,
        eventDuration = 30 * 60,
        includeWeekend = False,
        eventGenerationFlow = Period dayFrom dayTo,
        hourFrom = makeTimeOfDayValid 10 30 00,
        hourTo = makeTimeOfDayValid 11 50 00
    }
    let eventsList = concat $ map (getEventsOfTheDay hourFrom hourTo) (filterWeekend True [dayFrom .. dayTo])

    return $ fmap (createEvent 7) eventsList

main = print $ createEvents