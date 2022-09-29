module Repository.Subject (
    doesSubjectHasOngoingEventsInPeriod,
    insertEvent
) where

import Data.Time

-- TODO Get info from DB
doesSubjectHasOngoingEventsInPeriod :: Int -> (UTCTime, UTCTime) -> Bool
doesSubjectHasOngoingEventsInPeriod _ _ = False

-- TODO Do some SQL insert
insertEvent :: Int -> (UTCTime, UTCTime) -> Maybe ()
insertEvent subjectId (utcFrom, utcTo) = Just ()