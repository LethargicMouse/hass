module Display.Message
  ( Message (..),
  )
where

newtype Message = Message String

instance Show Message where
  show (Message s) = s
