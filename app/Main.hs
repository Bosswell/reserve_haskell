module Main where

import Control.Monad
import Data.Time
import Repository.Subject

data EventGenerationFlow = Select [Day] | Period Day Day deriving Show

data GenerateEvents = GenerateEvents {
    subjectId :: Int,
    eventDuration :: NominalDiffTime,
    eventGap :: NominalDiffTime,
    includeWeekend :: Bool,
    eventGenerationFlow :: EventGenerationFlow,
    hourFrom :: TimeOfDay,
    hourTo :: TimeOfDay
} deriving Show;

makeUTCTimeValid :: Day -> TimeOfDay -> UTCTime
makeUTCTimeValid d tod = UTCTime { utctDay = d, utctDayTime = timeOfDayToTime tod}

filterWeekend :: Bool -> [Day] -> [Day]
filterWeekend True days = days
filterWeekend _ days = filter (\day -> let dof = dayOfWeek day in dof `elem` [Saturday, Sunday]) days

createEvent :: Int -> (UTCTime, UTCTime) -> Maybe ()
createEvent subjectId eventPeriod | eventCanBeCreated == False = Nothing
                | otherwise = insertEvent subjectId eventPeriod
    where eventCanBeCreated = doesSubjectHasOngoingEventsInPeriod subjectId eventPeriod

getEventsOfTheDay :: GenerateEvents -> Day -> [(UTCTime, UTCTime)]
getEventsOfTheDay dto day = createEventsPeriods
        (makeUTCTimeValid day $ hourFrom dto)
        (makeUTCTimeValid day $ hourTo dto)
        (eventDuration dto)
        (eventGap dto)

createEventsPeriods :: UTCTime -> UTCTime -> NominalDiffTime -> NominalDiffTime -> [(UTCTime, UTCTime)]
createEventsPeriods utcFrom utcTo eventDuration eventGap
    | utcFrom >= utcTo = []
    | utcTo < utcToInPeriod = []
    | otherwise =
        [(utcFrom, utcToInPeriod)] ++
        createEventsPeriods (addUTCTime (eventDuration + eventGap) utcFrom) utcTo eventDuration eventGap
    where utcToInPeriod = addUTCTime (eventDuration - 1) utcFrom

createListWithDays :: EventGenerationFlow -> [Day]
createListWithDays (Period dayFrom dayTo) = filterWeekend True [dayFrom .. dayTo]
createListWithDays (Select days) = filterWeekend True days

createEvents :: GenerateEvents -> [Maybe ()]
createEvents dto = fmap (createEvent $ subjectId dto) (concat $ map (getEventsOfTheDay dto) (createListWithDays $ eventGenerationFlow dto))


normalizeMinutes :: Int -> Int
normalizeMinutes minutes
                | minutes `mod` 5 == 0 = minutes
                | otherwise = ((minutes `div` 5) + 1) * 5

normalizeHourFrom :: Int -> Int -> Maybe TimeOfDay
normalizeHourFrom hour minutes
    | normalizedMinutes == 60 = makeTimeOfDayValid (hour + 1) 00 00
    | otherwise = makeTimeOfDayValid hour normalizedMinutes 00
    where normalizedMinutes = normalizeMinutes minutes

f = do
    dayFrom <- fromGregorianValid 2010 3 4
    dayTo <- fromGregorianValid 2010 3 10

    hourFrom <- normalizeHourFrom 10 58
    hourTo <- makeTimeOfDayValid 11 50 00

    let dto = GenerateEvents {
        subjectId = 1,
        eventDuration = 30 * 60,
        eventGap = 0,
        includeWeekend = False,
        eventGenerationFlow = Period dayFrom dayTo,
        hourFrom = min hourFrom hourTo,
        hourTo = max hourFrom hourTo
    }

    return $ createEvents dto

main = print $ f