-- |
-- Module:     Network.FastIRC.Utils
-- Copyright:  (c) 2010 Ertugrul Soeylemez
-- License:    BSD3
-- Maintainer: Ertugrul Soeylemez <es@ertes.de>
-- Stability:  alpha
--
-- Utility functions for parsing IRC messages.

module Network.FastIRC.Utils
  ( -- * Character predicates for IRC
    isChannelChar,
    isChanPwdChar,
    isCommandChar,
    isHostChar,
    isIRCEOLChar,
    isIRCTokChar,
    isMessageChar,
    isNickChar,
    isServerChar,
    isUserChar,
    isUserSpecChar,

    -- * Other helper functions
    parseComplete
  )
  where

import qualified Data.ByteString.Char8 as B
import Data.Attoparsec.Char8


-- | Character predicate for channel names.

isChannelChar :: Char -> Bool
isChannelChar c = isIRCTokChar c && c /= ','


-- | Character predicate for channel passwords.

isChanPwdChar :: Char -> Bool
isChanPwdChar = isChannelChar


-- | Character predicate for IRC commands.

isCommandChar :: Char -> Bool
isCommandChar = inClass "A-Za-z0-9_"


-- | Character predicate for IRC user hostnames.  In the string @x!y\@z@
-- the substring @z@ is the user's hostname.

isHostChar :: Char -> Bool
isHostChar = isUserSpecChar


-- | Character predicate for IRC end of line characters.

isIRCEOLChar :: Char -> Bool
isIRCEOLChar c = c == '\n' || c == '\r'


-- | Character predicate for IRC tokens.

isIRCTokChar :: Char -> Bool
isIRCTokChar c = c /= ' ' && c /= '\r' && c /= '\n'


-- | Character predicate for IRC messages.

isMessageChar :: Char -> Bool
isMessageChar c = c /= '\n' && c /= '\r'


-- | Character predicate for IRC nicknames.  This function considers
-- high bytes (0x80 to 0xFF) and most nonstandard ASCII bytes as valid,
-- because most modern IRC daemons allow nonstandard nicknames.

isNickChar :: Char -> Bool
isNickChar = isUserSpecChar


-- | Character predicate for IRC servers.

isServerChar :: Char -> Bool
isServerChar c = inClass "a-zA-Z0-9.:-" c || c >= '\x80'


-- | Character predicate for IRC usernames.  In the string @x!y\@z@ the
-- substring @y@ is the user's username.

isUserChar :: Char -> Bool
isUserChar = isUserSpecChar


-- | Character predicate for nicknames, usernames and hostnames.

isUserSpecChar :: Char -> Bool
isUserSpecChar c = c > '!' && c /= '@'


-- | Run a parser completely.

parseComplete :: Parser a -> B.ByteString -> Maybe a
parseComplete p = complete . parse p
  where
    complete :: Result a -> Maybe a
    complete (Partial f)  = complete (f B.empty)
    complete (Done _ r)   = Just r
    complete (Fail _ _ _) = Nothing
