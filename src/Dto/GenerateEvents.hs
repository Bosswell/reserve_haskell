module Dto.GenerateEvents where

import Data.Time
import Data.DateTime

data GenerateEvents = GenerateEvents {
    subjectId :: Int,
    eventDuration :: Int,
    includeWeekend :: Bool,
    eventGenerationFlow :: Int,
    dateFrom :: Maybe DateTime,
    dateTo :: Maybe DateTime,
    hourFrom :: Maybe TimeOfDay,
    hourTo :: Maybe TimeOfDay,
    selectedDays :: [Maybe DateTime]
} deriving Show;

getDatePeriod :: Maybe Day -> Maybe Day -> Maybe [Day]
getDatePeriod (Just dateFrom) (Just dateTo) = Just [dateFrom .. dateTo]
getDatePeriod _ _ = Nothing


